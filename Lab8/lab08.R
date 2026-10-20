###############################################################################
# Lab 8: High-Performance Big Data Analytics Using R (NYC Yellow Taxi)
###############################################################################

# ------------------------------- 0. SETUP -----------------------------------
pkgs <- c("data.table", "arrow", "ggplot2", "purrr", "foreach",
          "doParallel", "microbenchmark", "scales")
new <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

theme_set(theme_minimal(base_size = 12))
set.seed(42)

# ------------------------ TASK 1: DATA ACQUISITION --------------------------
months   <- sprintf("2023-%02d", 1:3)           # change for more/less data
base_url <- "https://d37ci6vzurychx.cloudfront.net/trip-data/"
dir.create("data", showWarnings = FALSE)

files <- file.path("data", paste0("yellow_tripdata_", months, ".parquet"))
for (i in seq_along(files)) {
  if (!file.exists(files[i]))
    download.file(paste0(base_url, basename(files[i])), files[i], mode = "wb")
}
zone_file <- "data/taxi_zone_lookup.csv"
if (!file.exists(zone_file))
  download.file("https://d37ci6vzurychx.cloudfront.net/misc/taxi_zone_lookup.csv",
                zone_file, mode = "wb")

# Read only the columns needed (reduces memory)
cols <- c("tpep_pickup_datetime", "tpep_dropoff_datetime", "passenger_count",
          "trip_distance", "PULocationID", "DOLocationID", "payment_type",
          "fare_amount", "tip_amount", "tolls_amount", "total_amount")

trips <- rbindlist(lapply(files, function(f)
  as.data.table(arrow::read_parquet(f, col_select = cols))))

# 1.2 Inspect
cat("\n--- Structure ---\n");  str(trips)
cat("\n--- Dimensions ---\n"); print(dim(trips))
cat("\n--- Summary ---\n");    print(summary(trips))
cat("\n--- Missing values per column ---\n"); print(colSums(is.na(trips)))
cat("\n--- Memory (MB) before cleaning ---\n")
print(round(as.numeric(object.size(trips)) / 1024^2, 1))

# 1.3 Clean: duplicates, missing, invalid records
n0    <- nrow(trips)
trips <- unique(trips)
cat("Duplicates removed:", n0 - nrow(trips), "\n")

n1 <- nrow(trips)
trips <- trips[
  !is.na(passenger_count) &
    (passenger_count >= 1 & passenger_count <= 6) &
    (trip_distance > 0 & trip_distance < 100) &
    (fare_amount > 0 & fare_amount < 500) &
    total_amount > 0 & tip_amount >= 0 & tolls_amount >= 0 &
    tpep_dropoff_datetime > tpep_pickup_datetime &
    as.numeric(difftime(tpep_dropoff_datetime, tpep_pickup_datetime,
                        units = "hours")) < 6 &
    (PULocationID >= 1 & PULocationID <= 263) &
    (DOLocationID >= 1 & DOLocationID <= 263) &
    !is.na(payment_type)
]
cat("Invalid/missing/inconsistent records removed:", n1 - nrow(trips), "\n")

# 1.4 / 1.5 Data types and temporal attributes
pay_lab <- c("Flex Fare", "Credit card", "Cash", "No charge",
             "Dispute", "Unknown", "Voided")
trips[, `:=`(
  passenger_count = as.integer(passenger_count),
  PULocationID    = as.integer(PULocationID),
  DOLocationID    = as.integer(DOLocationID),
  payment         = factor(pay_lab[payment_type + 1L], levels = pay_lab),
  pickup_date     = as.IDate(tpep_pickup_datetime),
  hour            = hour(tpep_pickup_datetime),
  day             = mday(tpep_pickup_datetime),
  wday            = wday(tpep_pickup_datetime),      # 1 = Sunday
  month           = month(tpep_pickup_datetime),
  duration_min    = as.numeric(difftime(tpep_dropoff_datetime,
                                        tpep_pickup_datetime, units = "mins"))
)]
trips[, dow := factor(wday, 1:7, c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))]

# Keep only trips whose pickup falls in the selected months
trips <- trips[month %in% 1:3 & year(pickup_date) == 2023]

# 1.6 Drop unneeded columns to save memory
trips[, c("tpep_pickup_datetime", "tpep_dropoff_datetime", "payment_type") := NULL]
cat("\n--- Memory (MB) after cleaning ---\n")
print(round(as.numeric(object.size(trips)) / 1024^2, 1))
cat("Final rows:", nrow(trips), "\n")

# 1.7 / 1.8 Zone lookup + joins (data.table)
zones <- fread(zone_file)
setnames(zones, c("LocationID", "Borough", "Zone", "service_zone"))
zones[Zone == "" | is.na(Zone), Zone := "Unknown"]

trips[zones, on = .(PULocationID = LocationID),
      `:=`(PU_Borough = i.Borough, PU_Zone = i.Zone)]
trips[zones, on = .(DOLocationID = LocationID),
      `:=`(DO_Borough = i.Borough, DO_Zone = i.Zone)]
print(head(trips))

# ------------------------ TASK 2: TRANSPORT ANALYTICS -----------------------
# 2.1 Trips by hour (+ avg fare, revenue)
by_hour <- trips[, .(trips = .N, avg_fare = mean(fare_amount),
                     revenue = sum(total_amount)), by = hour][order(hour)]
print(by_hour)

# 2.2 Day of week
by_dow <- trips[, .(trips = .N, avg_fare = mean(fare_amount),
                    revenue = sum(total_amount)), by = dow][order(dow)]
print(by_dow)

# 2.3 / 2.4 Monthly
by_month <- trips[, .(trips = .N, avg_fare = mean(fare_amount),
                      revenue = sum(total_amount)), by = month][order(month)]
print(by_month)

by_day <- trips[, .(trips = .N, avg_fare = mean(fare_amount),
                    revenue = sum(total_amount)), by = pickup_date][order(pickup_date)]

# 2.5 / 2.6 Routes
routes <- trips[, .(trips = .N, revenue = sum(total_amount),
                    avg_fare = mean(fare_amount)),
                by = .(PU_Zone, DO_Zone)]
routes[, route := paste(PU_Zone, "->", DO_Zone)]
top_routes <- routes[order(-trips)][1:10]
rev_routes <- routes[order(-revenue)][1:10]
cat("\nMost frequent routes:\n");  print(top_routes)
cat("\nHighest-revenue routes:\n"); print(rev_routes)

top_pu <- trips[, .N, by = PU_Zone][order(-N)][1:10]
top_do <- trips[, .N, by = DO_Zone][order(-N)][1:10]

# 2.7 Distance vs fare
cat("\nCorrelation (distance, fare):",
    cor(trips$trip_distance, trips$fare_amount), "\n")
fit <- lm(fare_amount ~ trip_distance, data = trips[sample(.N, min(.N, 1e6))])
print(summary(fit))
dist_bins <- trips[, .(avg_fare = mean(fare_amount), trips = .N),
                   by = .(bin = cut(trip_distance, c(0,1,2,5,10,20,100)))][order(bin)]
print(dist_bins)

# 2.8 Payment by borough
pay_borough <- trips[, .N, by = .(PU_Borough, payment)]
pay_borough[, share := N / sum(N) * 100, by = PU_Borough]
print(pay_borough[order(PU_Borough, -N)])

# 2.9 Additional patterns
# (a) Average speed by hour
speed_hour <- trips[duration_min > 1,
                    .(avg_mph = mean(trip_distance / (duration_min / 60))),
                    by = hour][order(hour)]
# (b) Tip % for credit-card trips
tip_hour <- trips[payment == "Credit card",
                  .(tip_pct = mean(tip_amount / fare_amount) * 100),
                  by = hour][order(hour)]
# (c) Airport trips
ap_ids <- zones[grepl("Airport", Zone), LocationID]
trips[, airport := PULocationID %in% ap_ids | DOLocationID %in% ap_ids]
airport_cmp <- trips[, .(trips = .N, avg_fare = mean(fare_amount),
                         avg_dist = mean(trip_distance),
                         avg_tip = mean(tip_amount)), by = airport]
print(speed_hour); print(tip_hour); print(airport_cmp)

# ------------- TASK 3: FUNCTIONAL vs VECTORIZED vs DATA.TABLE ---------------
# Benchmark data: a random subset keeps repeated runs fast.
# Set N_BENCH <- nrow(trips) to use everything.
N_BENCH <- min(nrow(trips), 2e6)
bm   <- trips[sample(.N, N_BENCH), .(hour, fare_amount, trip_distance)]
fare <- bm$fare_amount
hr   <- bm$hour

# Operation A: mean fare for each hour (6 implementations)
A_aggregate <- function() aggregate(fare_amount ~ hour, data = as.data.frame(bm), FUN = mean)
A_tapply    <- function() tapply(fare, hr, mean)                         # vectorized base
A_lapply    <- function() unlist(lapply(0:23, function(h) mean(fare[hr == h])))
A_purrr     <- function() map_dbl(0:23, ~ mean(fare[hr == .x]))
A_dt        <- function() bm[, .(m = mean(fare_amount)), by = hour][order(hour)]$m

stopifnot(isTRUE(all.equal(unname(A_tapply()), A_dt())),
          isTRUE(all.equal(A_lapply(), A_dt())),
          isTRUE(all.equal(A_purrr(), A_dt())))

# Operation B: row-wise computation (fare per mile) on 100k rows
small <- bm[1:100000]
fa <- small$fare_amount; td <- small$trip_distance
m  <- cbind(fa, td)

B_apply  <- function() apply(m, 1, function(r) r[1] / r[2])
B_lapply <- function() unlist(lapply(seq_along(fa), function(i) fa[i] / td[i]))
B_purrr  <- function() map2_dbl(fa, td, ~ .x / .y)
B_vec    <- function() fa / td
B_dt     <- function() small[, fare_amount / trip_distance]

stopifnot(isTRUE(all.equal(unname(B_apply()), B_vec())),
          isTRUE(all.equal(B_lapply(), B_vec())),
          isTRUE(all.equal(B_purrr(), B_vec())),
          isTRUE(all.equal(B_dt(), B_vec())))

# Peak memory (MB) of each implementation
mem_peak <- function(f) {
  invisible(gc(reset = TRUE))
  base <- sum(gc(reset = TRUE)[, 6])
  invisible(f())
  round(sum(gc()[, 6]) - base, 1)
}
mem_table <- data.table(
  Operation = rep(c("A: mean fare by hour", "B: row-wise fare/mile"), c(5, 5)),
  Approach  = c("aggregate (base)", "tapply (vectorized)", "lapply", "purrr::map_dbl", "data.table",
                "apply", "lapply", "purrr::map2_dbl", "vectorized", "data.table"),
  Peak_MB   = c(mem_peak(A_aggregate), mem_peak(A_tapply), mem_peak(A_lapply),
                mem_peak(A_purrr), mem_peak(A_dt),
                mem_peak(B_apply), mem_peak(B_lapply), mem_peak(B_purrr),
                mem_peak(B_vec), mem_peak(B_dt)))
print(mem_table)

# ------------------------ TASK 5 (part 1): MICROBENCHMARK -------------------
mbA <- microbenchmark(aggregate_base = A_aggregate(), tapply_vectorized = A_tapply(),
                      lapply = A_lapply(), purrr_map = A_purrr(),
                      data_table = A_dt(), times = 10)
mbB <- microbenchmark(apply = B_apply(), lapply = B_lapply(), purrr_map2 = B_purrr(),
                      vectorized = B_vec(), data_table = B_dt(), times = 10)
print(mbA); print(mbB)
autoplot(mbA) + ggtitle("Operation A: mean fare by hour (log time)")
autoplot(mbB) + ggtitle("Operation B: row-wise fare per mile (log time)")

# ------------------------ TASK 4: PARALLEL COMPUTING ------------------------
# Partition by pickup date (about 90 partitions; use by = "month" for month-wise)
parts <- split(trips[, .(pickup_date, PULocationID, DOLocationID,
                         fare_amount, tip_amount, trip_distance)],
               by = "pickup_date")
cat("\nNumber of partitions:", length(parts), "\n")

# Computationally intensive task per partition:
# bootstrap CI of mean fare + regression + route aggregation
heavy_task <- function(dt, B = 200) {
  data.table::setDTthreads(1)          # avoid thread oversubscription in workers
  set.seed(as.integer(dt$pickup_date[1]))
  n    <- nrow(dt)
  boot <- replicate(B, mean(sample(dt$fare_amount, n, replace = TRUE)))
  fit  <- lm(fare_amount ~ trip_distance, data = dt)
  top  <- dt[, .N, by = .(PULocationID, DOLocationID)][which.max(N)]
  data.table(date = dt$pickup_date[1], n = n,
             mean_fare = mean(dt$fare_amount),
             ci_low = unname(quantile(boot, .025)),
             ci_high = unname(quantile(boot, .975)),
             slope = coef(fit)[2],
             top_route_trips = top$N)
}

# A. Sequential
t_seq <- system.time(res_seq <- rbindlist(lapply(parts, heavy_task)))[["elapsed"]]
cat("Sequential time (s):", t_seq, "\n")

# B. Parallel (foreach + doParallel)
run_parallel <- function(n_cores) {
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  on.exit({ stopCluster(cl); registerDoSEQ() })
  tm <- system.time(
    res <- foreach(p = parts, .packages = "data.table",
                   .export = "heavy_task") %dopar% heavy_task(p)
  )[["elapsed"]]
  list(time = tm, res = rbindlist(res))
}

max_cores  <- max(1, parallel::detectCores() - 1)
core_set   <- unique(c(2, max_cores)); core_set <- core_set[core_set <= max(2, max_cores)]
par_results <- lapply(core_set, run_parallel)

speed_table <- data.table(
  Cores      = c(1, core_set),
  Time_sec   = c(t_seq, sapply(par_results, `[[`, "time"))
)
speed_table[, Speedup    := round(t_seq / Time_sec, 2)]
speed_table[, Efficiency := round(Speedup / Cores, 2)]
print(speed_table)

# Sanity check: parallel gives same means as sequential
stopifnot(isTRUE(all.equal(res_seq$mean_fare, par_results[[1]]$res$mean_fare)))

ggplot(speed_table, aes(factor(Cores), Time_sec)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(Time_sec, 1), "s\n", Speedup, "x")), vjust = -0.2) +
  labs(title = "Sequential vs Parallel Execution Time",
       x = "Number of cores", y = "Elapsed time (seconds)") +
  expand_limits(y = max(speed_table$Time_sec) * 1.15)

# ------------------- TASK 5 (part 2): COMPARISON TABLE ----------------------
sm <- function(mb, grp) {
  s <- summary(mb, unit = "ms")
  data.table(Comparison = grp, Approach = as.character(s$expr), Median_ms = round(s$median, 2))
}
best_par <- speed_table[Cores > 1][which.min(Time_sec)]
bench_table <- rbindlist(list(
  sm(mbA, paste0("A: mean fare by hour (", format(N_BENCH, big.mark = ","), " rows)")),
  sm(mbB, "B: row-wise fare/mile (100k rows)"),
  data.table(Comparison = "C: bootstrap per date-partition",
             Approach   = c("sequential", paste0("parallel (", best_par$Cores, " cores)")),
             Median_ms  = round(c(t_seq, best_par$Time_sec) * 1000, 2))
))
bench_table[, Relative_to_fastest := round(Median_ms / min(Median_ms), 1), by = Comparison]
cat("\n===== EXECUTION-TIME COMPARISON TABLE =====\n")
print(bench_table)

# system.time() example for full data (data.table vs base)
cat("\nFull-data system.time():\n")
print(system.time(trips[, .(m = mean(fare_amount)), by = .(PU_Borough, hour)]))
print(system.time(tapply(trips$fare_amount, list(trips$PU_Borough, trips$hour), mean)))

# ------------------------ TASK 6: VISUALIZATIONS ----------------------------
dir.create("plots", showWarnings = FALSE)
show <- function(p, name) { print(p); ggsave(file.path("plots", paste0(name, ".png")), p, width = 9, height = 5, dpi = 120) }

show(ggplot(by_hour, aes(hour, trips)) + geom_col(fill = "steelblue") +
       scale_y_continuous(labels = comma) + scale_x_continuous(breaks = 0:23) +
       labs(title = "Taxi Trips by Hour of Day", x = "Hour (0-23)", y = "Number of trips"), "p01_hour")

show(ggplot(by_dow, aes(dow, trips, fill = dow)) + geom_col(show.legend = FALSE) +
       scale_y_continuous(labels = comma) +
       labs(title = "Taxi Demand by Day of Week", x = "Day of week", y = "Number of trips"), "p02_dow")

show(ggplot(by_month, aes(factor(month, labels = month.abb[sort(unique(by_month$month))]), trips)) +
       geom_col(fill = "darkorange") + scale_y_continuous(labels = comma) +
       labs(title = "Monthly Taxi Demand", x = "Month (2023)", y = "Number of trips"), "p03_month")

show(ggplot(by_hour, aes(hour, avg_fare)) + geom_line(colour = "firebrick", linewidth = 1) + geom_point() +
       scale_x_continuous(breaks = 0:23) +
       labs(title = "Average Fare by Hour", x = "Hour", y = "Average fare (USD)"), "p04_avg_fare_hour")

show(ggplot(by_day, aes(pickup_date, revenue / 1e6)) + geom_line(colour = "darkgreen") +
       labs(title = "Daily Revenue Trend", x = "Date", y = "Total revenue (million USD)"), "p05_revenue_daily")

show(ggplot(top_pu, aes(reorder(PU_Zone, N), N)) + geom_col(fill = "steelblue") + coord_flip() +
       scale_y_continuous(labels = comma) +
       labs(title = "Top 10 Pickup Zones", x = "Pickup zone", y = "Number of trips"), "p06_top_pickup")

show(ggplot(top_do, aes(reorder(DO_Zone, N), N)) + geom_col(fill = "purple") + coord_flip() +
       scale_y_continuous(labels = comma) +
       labs(title = "Top 10 Drop-off Zones", x = "Drop-off zone", y = "Number of trips"), "p07_top_dropoff")

show(ggplot(top_routes, aes(reorder(route, trips), trips)) + geom_col(fill = "teal") + coord_flip() +
       scale_y_continuous(labels = comma) +
       labs(title = "Top 10 Most Frequent Routes", x = "Route", y = "Number of trips"), "p08_top_routes")

show(ggplot(rev_routes, aes(reorder(route, revenue), revenue / 1e3)) + geom_col(fill = "goldenrod") + coord_flip() +
       labs(title = "Top 10 Highest-Revenue Routes", x = "Route", y = "Total revenue (thousand USD)"), "p09_rev_routes")

pay_total <- trips[, .N, by = payment][, share := N / sum(N) * 100]
show(ggplot(pay_total, aes(reorder(payment, -share), share, fill = payment)) + geom_col(show.legend = FALSE) +
       labs(title = "Payment-Type Distribution", x = "Payment type", y = "Share of trips (%)"), "p10_payment")

show(ggplot(pay_borough, aes(PU_Borough, share, fill = payment)) + geom_col() + coord_flip() +
       labs(title = "Payment Type by Pickup Borough", x = "Borough", y = "Share of trips (%)", fill = "Payment"), "p11_payment_borough")

smp <- trips[trip_distance <= 30][sample(.N, min(.N, 50000))]
show(ggplot(smp, aes(trip_distance, fare_amount)) + geom_point(alpha = 0.1, colour = "steelblue") +
       geom_smooth(method = "lm", colour = "red") +
       labs(title = "Trip Distance vs Fare Amount (50k sample)",
            x = "Trip distance (miles)", y = "Fare amount (USD)"), "p12_dist_fare")

show(ggplot(speed_hour, aes(hour, avg_mph)) + geom_line(linewidth = 1, colour = "navy") + geom_point() +
       scale_x_continuous(breaks = 0:23) +
       labs(title = "Average Trip Speed by Hour", x = "Hour", y = "Speed (mph)"), "p13_speed")

# ----------------------- TASK 7: SUPPORTING NUMBERS -------------------------
cat("\n===== NUMBERS TO CITE IN YOUR CRITICAL ANALYSIS =====\n")
print(bench_table)
print(mem_table)
print(speed_table)
cat("Fastest overall (Op A):", bench_table[grepl("^A", Comparison)][which.min(Median_ms), Approach], "\n")
cat("Fastest overall (Op B):", bench_table[grepl("^B", Comparison)][which.min(Median_ms), Approach], "\n")
cat("Best parallel speedup:", max(speed_table$Speedup), "x with",
    speed_table[which.max(Speedup), Cores], "cores\n")


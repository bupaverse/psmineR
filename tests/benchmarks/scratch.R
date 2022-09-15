x <- data.table(X = sample.int(1000, 10000, replace = TRUE),
                Y = sample.int(1000, 10000, replace = TRUE)) %>%
  mutate(across(.cols = "X",
                .fns = ~ ifelse(row_number(.x) %in% sample(1:n(), size = ((0.25 * 100) * n() / 100)), NA, .x)))

# na.omit is fastest
bench::mark(
  na.omit = na.omit(x, cols = "X"),
  stats_na.omit = stats::na.omit(x),
  is.na = x[!is.na(x$X),],
  which = x[-which(is.na(x$X)),],
  iterations = 1000,
  check = TRUE
)

# A tibble: 4 x 13
#expression         min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result                   memory              time               gc
#<bch:expr>    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>                   <list>              <list>             <list>
#1 na.omit          163us    274us     3249.     182KB    19.6    994     6      306ms <data.table [7,500 x 2]> <Rprofmem [7 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#2 stats_na.omit    192us    307us     3076.     182KB    18.6    994     6      323ms <data.table [7,500 x 2]> <Rprofmem [7 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#3 is.na            362us    589us     1601.     222KB    11.3    993     7      620ms <data.table [7,500 x 2]> <Rprofmem [9 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#4 which            758us    988us      992.     202KB     4.98   995     5         1s <data.table [7,500 x 2]> <Rprofmem [10 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>

x <- data.table(X = sample.int(1000, 10000, replace = TRUE),
                Y = sample.int(10, 10000, replace = TRUE))
q <- quantile(x$X, probs = c(0.25, 0.5, 0.75))

# fcase is much faster
bench::mark(
  if_else = x[, C := if_else(X <= q[1], 1L, if_else(X <= q[2], 2L, if_else(X <= q[3], 3L, 4L))), by = "Y"],
  ifelse = x[, C := ifelse(X <= q[1], 1L, ifelse(X <= q[2], 2L, ifelse(X <= q[3], 3L, 4L))), by = "Y"],
  fcase = x[, C := fcase(X <= q[1], 1L, X <= q[2], 2L, X <= q[3], 3L, default = 4L)],
  case_when = x[, C := case_when(X <= q[1] ~ 1L, X <= q[2] ~ 2L, X <= q[3] ~ 4L, TRUE ~ 4L)],
  subset = x[, C := 0L][X <= q[1], C := 1L, by = "Y"][X <= q[2], C := 2L, by = "Y"][X <= q[3], C := 3L, by = "Y"],
  iterations = 1000,
  check = TRUE
)

# A tibble: 5 x 13
#expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result                    memory               time               gc
#<bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>                    <list>               <list>             <list>
#1 if_else      3.72ms   4.74ms      208.     1.8MB     9.56   956    44      4.61s <data.table [10,000 x 3]> <Rprofmem [502 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#2 ifelse       2.88ms   3.75ms      266.    1.25MB     8.80   968    32      3.63s <data.table [10,000 x 3]> <Rprofmem [373 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#3 fcase       523.6us 606.85us     1536.  228.07KB     9.27   994     6   647.33ms <data.table [10,000 x 3]> <Rprofmem [10 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#4 case_when    1.64ms    2.1ms      455.    1.79MB    21.4    955    45       2.1s <data.table [10,000 x 3]> <Rprofmem [84 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#5 subset       4.77ms   5.27ms      181.  669.88KB     4.06   978    22      5.42s <data.table [10,000 x 3]> <Rprofmem [62 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>


x <- data.table(X = sample.int(10000, 10000, replace = TRUE))
y <- copy(x)
z <- copy(x)
w <- copy(x)

# setorderv_subset is fastest
bench::mark(
  setorderv_subset = {setorderv(y, cols = "X", order = -1L)
                      y[1:5]},
  setorderv_head = {setorderv(z, cols = "X", order = -1L)
                    z[, head(.SD, 5)]},
  setorderv_SD = {setorderv(w, cols = "X", order = -1L)
                  w[, .SD[1:5]]},
  order_head = x[order(-X), head(.SD, 5)],
  order_SD = x[order(-X), .SD[1:5]],
  order_I = {setorderv(y, cols = "X", order = -1L)
             y[y[order(-X), .I[1:5]]]},
  iterations = 1000,
  check = TRUE
)

# A tibble: 6 x 13
#expression            min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
#<bch:expr>       <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>
#1 setorderv_subset  400.7us  488.9us     1645.     104KB     1.65   999     1
#2 setorderv_head    929.2us   1.02ms      895.     153KB     2.69   997     3
#3 setorderv_SD      858.4us  956.4us      961.     153KB     2.89   997     3
#4 order_head         1.26ms   1.57ms      614.     127KB     1.85   997     3
#5 order_SD           1.19ms   1.46ms      668.     127KB     2.01   997     3
#6 order_I             1.2ms   1.31ms      713.     150KB     2.86   996     4


x <- data.table(X = sample.int(20000, 10000, replace = FALSE),
                Y = sample.int(10000, 10000, replace = TRUE))
y <- data.table(X = sample.int(20000, 10000, replace = FALSE),
                Y = sample.int(10000, 10000, replace = TRUE))

# left_join: keys is fastest
bench::mark(
  on = y[x, on = "X"],
  merge = merge(x, y, all.x = TRUE, by = "X"),
  left_join = left_join(x, y, by = "X"),
  keys = {setkeyv(x, cols = "X")
          setkeyv(y, cols = "X")
          y[x]},
  iterations = 1000,
  check = FALSE
)

# A tibble: 4 x 13
#expression      min median itr/s~1 mem_al~2 gc/se~3 n_itr  n_gc total~4 result
#<bch:expr> <bch:tm> <bch:>   <dbl> <bch:by>   <dbl> <int> <dbl> <bch:t> <list>
#1 on           1.92ms 3.05ms    325. 463.98KB    2.29   993     7   3.05s <NULL>
#2 merge        2.31ms 2.99ms    315. 552.02KB    2.54   992     8   3.15s <NULL>
#3 left_join    3.43ms 3.87ms    244.   1.14MB    6.01   976    24      4s <NULL>
#4 keys          1.3ms 1.78ms    523. 514.65KB    4.22   992     8    1.9s <NULL>
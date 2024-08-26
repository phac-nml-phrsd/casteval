# mean-and-quantiles
groups1 <- dplyr::tribble(
    ~time,  ~grp_variable,  ~grp_province,  ~grp_scenario,  ~val_q5,    ~val_q95,   ~val_mean,
    1,      "hosp",         "ON",           1,              10,         20,         15,
    1,      "case",         "ON",           1,              1000,       2000,       1500,
    1,      "hosp",         "QC",           1,              11,         21,         16,
    1,      "case",         "QC",           1,              1100,       2100,       1600,
    1,      "hosp",         "ON",           2,              15,         25,         20,
    1,      "case",         "ON",           2,              1500,       2500,       2000,
    1,      "hosp",         "QC",           2,              16,         26,         21,
    1,      "case",         "QC",           2,              1600,       2600,       2100,
    2,      "hosp",         "ON",           1,              50,         60,         55,
    2,      "case",         "ON",           1,              5000,       6000,       5500,
    2,      "hosp",         "QC",           1,              51,         61,         56,
    2,      "case",         "QC",           1,              5100,       6100,       5600,
    2,      "hosp",         "ON",           2,              55,         65,         60,
    2,      "case",         "ON",           2,              5500,       6500,       6000,
    2,      "hosp",         "QC",           2,              56,         66,         61,
    2,      "case",         "QC",           2,              5600,       6600,       6100
)

# raw
groups2 <- dplyr::tribble(
    ~time,  ~grp_variable,  ~grp_province,  ~grp_scenario,  ~val,
    1,      "hosp",         "ON",           1,              15,
    1,      "hosp",         "ON",           1,              15,
    1,      "case",         "ON",           1,              1499,
    1,      "case",         "ON",           1,              1501,
    1,      "hosp",         "QC",           1,              14,
    1,      "hosp",         "QC",           1,              18,
    1,      "case",         "QC",           1,              1597,
    1,      "case",         "QC",           1,              1603,
    1,      "hosp",         "ON",           2,              16,
    1,      "hosp",         "ON",           2,              24,
    1,      "case",         "ON",           2,              1995,
    1,      "case",         "ON",           2,              2005,
    1,      "hosp",         "QC",           2,              15,
    1,      "hosp",         "QC",           2,              27,
    1,      "case",         "QC",           2,              2092,
    1,      "case",         "QC",           2,              2108,
    2,      "hosp",         "ON",           1,              46,
    2,      "hosp",         "ON",           1,              64,
    2,      "case",         "ON",           1,              5490,
    2,      "case",         "ON",           1,              5510,
    2,      "hosp",         "QC",           1,              1,
    2,      "hosp",         "QC",           1,              2,
    2,      "case",         "QC",           1,              3,
    2,      "case",         "QC",           1,              4,
    2,      "hosp",         "ON",           2,              5,
    2,      "hosp",         "ON",           2,              6,
    2,      "case",         "ON",           2,              7,
    2,      "case",         "ON",           2,              8,
    2,      "hosp",         "QC",           2,              9,
    2,      "hosp",         "QC",           2,              10,
    2,      "case",         "QC",           2,              11,
    2,      "case",         "QC",           2,              12
)

# observations
groups_obs <- dplyr::tribble(
    ~time,  ~grp_variable,  ~grp_province,  ~grp_scenario,  ~val_obs,
    1,      "hosp",         "ON",           1,              9,
    1,      "case",         "ON",           1,              1000,
    1,      "hosp",         "QC",           1,              12,
    1,      "case",         "QC",           1,              1599,
    1,      "hosp",         "ON",           2,              20,
    1,      "case",         "ON",           2,              2001,
    1,      "hosp",         "QC",           2,              24,
    1,      "case",         "QC",           2,              2500,
    2,      "hosp",         "ON",           1,              27,
    2,      "case",         "ON",           1,              10,
    2,      "hosp",         "QC",           1,              11,
    2,      "case",         "QC",           1,              12,
    2,      "hosp",         "ON",           2,              13,
    2,      "case",         "ON",           2,              14,
    2,      "hosp",         "QC",           2,              15,
    2,      "case",         "QC",           2,              16
)

usethis::use_data(groups1, overwrite=TRUE)
usethis::use_data(groups2, overwrite=TRUE)
usethis::use_data(groups_obs, overwrite=TRUE)
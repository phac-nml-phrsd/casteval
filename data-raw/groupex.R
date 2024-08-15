groupex <- dplyr::tribble(
    ~time,  ~grp_variable,  ~grp_province,  ~grp_scenario,  ~val_q5,    ~val_q95,
    1,      "hosp",         "ON",           1,              10,         20,
    1,      "case",         "ON",           1,              1000,       2000,
    1,      "hosp",         "QC",           1,              11,         21,
    1,      "case",         "QC",           1,              1100,       2100,
    1,      "hosp",         "ON",           2,              15,         25,
    1,      "case",         "ON",           2,              1500,       2500,
    1,      "hosp",         "QC",           2,              16,         26,
    1,      "case",         "QC",           2,              1600,       2600,
    2,      "hosp",         "ON",           1,              50,         60,
    2,      "case",         "ON",           1,              5000,       6000,
    2,      "hosp",         "QC",           1,              51,         61,
    2,      "case",         "QC",           1,              5100,       6100,
    2,      "hosp",         "ON",           2,              55,         65,
    2,      "case",         "ON",           2,              5500,       6500,
    2,      "hosp",         "QC",           2,              56,         66,
    2,      "case",         "QC",           2,              5600,       6600
)

usethis::use_data(groupex, overwrite=TRUE)
# membuat dataframenya
data_covid <- data.frame(Country = c("USA", "India", "Brazil",
                                    "Russian", "UK", "France"),
                         Cases_Cumulative = c(27491574, 10950201, 9921981,
                                            4125598, 4071189, 3453645),
                         Death_Cumulative = c(485379, 156014, 240940,
                                            81926, 118933, 82692))

# membuat rata rata kasus
mean_cases_cumulative <- mean(data_covid$Cases_Cumulative)
print(paste("rata rata pada kolom Cases Cumulative", mean_cases_cumulative))

# membuat banyaknya seluruh kematian
sum_death_cumulative <- sum(data_covid$Death_Cumulative)
print(paste("total pada kolom Death Cumulative", sum_death_cumulative))

#membuat data_covid kolom country berurut (A -> Z)
data_covid_urut <- data_covid[order(data_covid$Country), ]
print(data_covid_urut)
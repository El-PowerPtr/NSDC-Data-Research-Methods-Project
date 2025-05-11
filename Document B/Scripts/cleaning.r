replace <- function(table, field, dictionary) {
    Map(
        function(x) {
            if (is.list(dictionary)) {
                if (x %in% names(dictionary)) {
                    dictionary[[toString(x)]]
                } else {
                    x
                }
            } else {
                dictionary[as.integer(x)]
            }
        },
        table[[field]]
    )
}

replace_if_in <- function(table, field, dictionary) {
    for (i in seq_along(dictionary)) {
        table[[field]] <- Map(
            function(x) {
                if (x %in% dictionary[[i]]) {
                    i
                } else {
                    x
                }
            },
            table[[field]]
        )
    }
    table[[field]]
}
names <- names(read.csv("../DataSets/Milestone4Data.csv"))
dataset <- read.csv("../DataSets/Milestone4Data.csv", skip = 2)
dataset_no_duplicates <- unique.data.frame(dataset)
dataset_no_duplicates$EST_MSA <- Map(
    function(x) {
        if (length(x) == 1) {
            "35620"
        } else {
            x
        }
    },
    dataset_no_duplicates$EST_MSA
)
EST_MSA_map <- list(
    "35620" = "New York-Newark-Jersey City, NY-NJ-PA Metro Area",
    "31080" = "Los Angeles-Long Beach-Anaheim, CA Metro Are",
    "16980" = "Chicago-Naperville-Elgin, IL-IN-WI Metro Are",
    "19100" = "Dallas-Fort Worth-Arlington, TX Metro Area",
    "26420" = "Houston-The Woodlands-Sugar Land, TX Metro Area",
    "47900" = "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area",
    "33100" = "Miami-Fort Lauderdale-Pompano Beach, FL Metro Area",
    "37980" = "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD Metro Area",
    "12060" = "Atlanta-Sandy Springs-Alpharetta, GA Metro Area",
    "38060" = "Phoenix-Mesa-Chandler, AZ Metro Area",
    "14460" = "Boston-Cambridge-Newton, MA-NH Metro Area",
    "41860" = "San Francisco-Oakland-Berkeley, CA Metro Area",
    "40140" = "Riverside-San Bernardino-Ontario, CA Metro Area",
    "19820" = "Detroit-Warren-Dearborn, MI Metro Area",
    "42660" = "Seattle-Tacoma-Bellevue, WA Metro Area"
)
dataset_no_duplicates$EST_MSA <- replace(
    dataset_no_duplicates,
    "EST_MSA",
    EST_MSA_map
)
regions <- list(
    c(09, 23, 25, 33, 34, 36, 42, 44, 50),
    c(01, 05, 10, 11, 12, 13, 21, 22, 24, 28, 37, 40, 45, 47, 48, 51, 54),
    c(18, 17, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55),
    c(02, 04, 06, 08, 15, 16, 30, 32, 35, 41, 49, 53, 56)
)
dataset_no_duplicates$REGION <- replace_if_in(
    dataset_no_duplicates,
    "EST_ST",
    regions
)
dataset_no_duplicates$REGION <- replace(
    dataset_no_duplicates,
    "REGION",
    c(
        "Northeast",
        "South",
        "Midwest",
        "West"
    )
)
EST_ST_map <- list(
    "1" = "Alabama",
    "2" = "Alaska",
    "4" = "Arizona",
    "5" = "Arkansas",
    "6" = "California",
    "8" = "Colorado",
    "9" = "Connecticut",
    "10" = "Delaware",
    "11" = "District of Columbia",
    "12" = "Florida",
    "13" = "Georgia",
    "15" = "Hawaii",
    "16" = "Idaho",
    "17" = "Illinois",
    "18" = "Indiana",
    "19" = "Iowa",
    "20" = "Kansas",
    "21" = "Kentucky",
    "22" = "Louisiana",
    "23" = "Maine",
    "24" = "Maryland",
    "25" = "Massachusetts",
    "26" = "Michigan",
    "27" = "Minnesota",
    "28" = "Mississippi",
    "29" = "Missouri",
    "30" = "Montana",
    "31" = "Nebraska",
    "32" = "Nevada",
    "33" = "New Hampshire",
    "34" = "New Jersey",
    "35" = "New Mexico",
    "36" = "New York",
    "37" = "North Carolina",
    "38" = "North Dakota",
    "39" = "Ohio",
    "40" = "Oklahoma",
    "41" = "Oregon",
    "42" = "Pennsylvania",
    "44" = "Rhode Island",
    "45" = "South Carolina",
    "46" = "South Dakota",
    "47" = "Tennessee",
    "48" = "Texas",
    "49" = "Utah",
    "50" = "Vermont",
    "51" = "Virginia",
    "53" = "Washington",
    "54" = "West Virginia",
    "55" = "Wisconsin",
    "56" = "Wyoming"
)
dataset_no_duplicates$EST_ST <- replace(
    dataset_no_duplicates,
    "EST_ST",
    EST_ST_map
)
dataset_no_duplicates$RHISPANIC <- replace(
    dataset_no_duplicates,
    "RHISPANIC",
    c("No", "Yes")
)
dataset_no_duplicates$RRACE <- replace(
    dataset_no_duplicates,
    "RRACE",
    c("White", "Black", "Asian", "Other")
)
education <- c(
    "Less than high school",
    "Some high school",
    "High school graduate or equivalent (for example GED)",
    "Some college, but degree not received or is in progress",
    "Associate's degree (for example AA, AS)",
    "Bachelor's degree (for example BA, BS, AB)",
    "Graduate degree (for example master's, professional, doctorate)"
)
dataset_no_duplicates$EEDUC <- replace(
    dataset_no_duplicates,
    "EEDUC",
    education
)
dataset_no_duplicates$TBIRTH_YEAR <- Map(
    function(x) {
        x <- substr(x, 1, 4)
        x
    },
    dataset_no_duplicates$TBIRTH_YEAR
)
names <- Map(
    function(x) {
        if (grepl("X", x)) {
            ""
        } else {
            x
        }
    },
    names
)
write.table(
    names,
    "../Answers/../Answers/Milestone4Data (clean).csv",
    col.names = FALSE,
    row.names = FALSE,
    sep = ",",
    quote = FALSE
)
dataset_no_duplicates <- apply(dataset_no_duplicates, 2, unlist)
write.table(
    dataset_no_duplicates,
    "../Answers/Milestone4Data (clean).csv",
    row.names = FALSE,
    sep = ",",
    append = TRUE,
    quote = FALSE
)

#Problem 1
#Part A
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)


#Part A
section <- factor(x = section, levels = c('A','B'))

students <- data.frame(student_id, section, quiz1, quiz2, passed)

score_matrix <- matrix(
  c(quiz1,quiz2), 
  nrow = 6, ncol = 2, 
  dimnames = list(student_id, c("quiz1", "quiz2"))
)

course_record = list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)

#Vectors and Lists are one dimensional, 
#Matrix and Dataframes are both two dimensional data structures, arrays are three dimensional structures. 
#Vectors, Matrix, and Arrays can only store one data type, whereas Lists and Dataframes can handle 
#multiple data types at a time. Factors are unique because they are used to store and describe categorical data. 

#Part B
so4_score <- score_matrix[4,2]

first_scores <- score_matrix[1:2, ,drop = FALSE]


one_bracket <- course_record['course']

two_bracket <- course_record[['course']]

dollar_sign <- course_record$course

#one bracket returns a list
#two brackets returns the elements of that list
#the dollar sign in this instance accesses course, 
#but it can also be used to add/delete and manipulate data 

#Part C
students$average <- rowMeans(students[, c('quiz1', 'quiz2')])

students$excellent <- students$average >= 90

section_a_high <- students[students$section == "A" & students$average >= 80,
                           c("student_id", "section", "average")]

student_average <- setNames(students$average, students$student_id)
#the calculations remain vectorized because the $ operator accesses columns in 
#dataframes. A column in a dataframe is basically a vector.

#Problem 2
#Part A

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

measurements <- read.csv(text = csv_text, na.strings = c("","NA"))

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

sum(is.na(measurements))

filter <- complete.cases(measurements)
measurements_complete <- measurements[filter,]

measurements$sample_id[!filter]

#because NA is considered no value, there is nothing for R to compare it too
#so any comparisons made wouldn't return a boolean but rather NA


#Part B
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)
levels(measurements$sites)
levels(measurements$status)

measurements$temp_f = measurements$temp_c * (9/5) + 32

measurements$ph_below_7 <- measurements$ph < 7

keep <- complete.cases(measurements) &
  measurements$site %in% c("North", "South") &
  measurements$status == "ok"

selected <- measurements[keep, c("sample_id", "site", "temp_c", "temp_f", "ph")]
selected

mean(measurements$temp_c, na.rm = TRUE)

mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)

#Part C
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

element_wise = A*B
matrix_mult = A%*%B
#elementwise multiplication only multiplies the values in corresponding positions of the two matrices
#matrix multiplication computes the cross product of A and B

#Problem 3
#Part A
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
){
  if (is.na(score)){
    return (NA_character_)}
  
  else if (score >= a_min){
     "A"}
  
  else if (score >= b_min){
     "B"}
  
  else if (score >= c_min){
     "C"}
  
  else if (score >= d_min){
     "D"}
  
  else{
    "F"}
}
  
grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

#Part B

grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id
#During the loop, i is the position of the current element in scores, 
#running from 1 to 6, so scores[i] is read, grades[i] is written, and both line up with student_id[i]


#Part C-1
summarize_scores <- function(x, na.rm = TRUE, digits = 1){
  c(
    n_total<-length(x),
    n_missing <- sum(is.na(x)),
    mean<- round(mean(x, na.rm = na.rm), digits),
    sd <- round(sd(x, na.rm = na.rm), digits),
    min <- round(min(x, na.rm = na.rm), digits),
    max <- round(max(x, na.rm = na.rm), digits)
  )
}

summarize_scores(x = scores, na.rm = TRUE, digits = 2)

#Part C-2
plot_scores <- function(x, ...) {
  defaults <- list(
    x    = seq_along(x),
    y    = x,
    xlab = "Position",
    ylab = "Score",
    pch  = 19
  )
  do.call(plot, modifyList(defaults, list(...)))
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores" )

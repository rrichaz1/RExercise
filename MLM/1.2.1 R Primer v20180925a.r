####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# 1.2.1 R Primer
# Introduction to R scripting standards and basic concepts
# Marc d. Paradis
# R 3.4.1 for Windows
# RStudio 1.1.383

# The hash is used for comments. This line of code will not be run.

# Variables
# characters
a <- 'alpha'  # Unlike many other languages, R does not make a disinction 
              # between single and double quotes.
b <- '1'
c <- as.character(NA)  # as.character() is a function. The values within 
                       # the parens, in this case NA, are known as arguments.
                       # The function as.character() takes an argument and 
                       # converts it to a character datatype.
d <- as.character(NULL) # NA and NULL are recognized by R as having special 
                        # meaning
e <- As.character(NULL) # R is case-sensitive, this is the source of much
                        # frustration. Pay careful attention to case.

# numbers
integer <- as.integer(1)
integer.l <- 2L
real <- 22/7
cmplx <- 2 + 1i
infinity <- 1/0
missing.num <- as.numeric(NA)
missing.int <- as.integer(NA)
null.num <- as.numeric(NULL)
null.int <- as.integer(NULL)

# Logical/Boolean
bool <- TRUE
bool.na <- as.logical(NA)
bool.null <- as.logical(NULL)

# Factor
factor.num <- as.factor(1)
factor.char <- as.factor('red')
factor.na <- as.factor(NA)
factor.null <- as.factor(NULL)

# Example
pt01.systolic <- 147
pt01.diastolic <- 90
pt01.hypertension <- TRUE
pt01.visit <- 1L
pt01.hyp.meds <- 'HCTZ'

# Vectors
vc.greek <- c('alpha', 'beta', 'gamma')
vc.greek.na <- c('alpha', NA, 'gamma')
vc.greek.null <- c('alpha', NULL, 'gamma')

vi.integers <- c(1L, 2L, 3L)
vi.integers.na <- c(1L, NA, 3L)
vi.integers.null <- c(1L, NULL, 3L)

vn.numbers <- c(1, 2, 3)
vn.numbers.na <- c(1, NA, 3)
vn.numbers.null <- c(1, NULL, 3)

vl.bool <- c(TRUE, FALSE, FALSE)
vl.bool.na <- c(TRUE, NA, FALSE)
vl.bool.null <- c(TRUE, NULL, FALSE)

vc.coercion <- c(1, 2L, '3')
vn.coercion <- c(1, 2L, 3)
vn.coercion <- c(TRUE, 2L, 3)
vn.coercion <- c(FALSE, 2L, 3)
vc.coercion <- c(TRUE, 'FALSE')

one.dim.space <- 
  as.integer(
    c(
      9,7,4,3,9,6,11,7,8,5,8,10,2,7,4,6,9,5,5,4,3,11,8,7,9,8,6,10,7,6,7,8,5,6,
      10,12
      )
    )

# Let's take a look at our one dimensional space

print(one.dim.space)  # the print function will print in the console window 
                      # below
one.dim.space   # simply typing and running an object is an implicit print 
                # command

# To view the online documentation for a function, you can precede the function
# name with a question mark. For example, to view the docs on the print
# function type the following:

?print

# or type "print" into the search field in the help tab in the lower right

# Let's try to understand some of the characteristics of our one.dim.space 
# object

is.vector(one.dim.space)      # is it a vector (v.)
is.matrix(one.dim.space)      # is it a matrix (mx.)
is.array(one.dim.space)       # is it an array (ar.)
is.list(one.dim.space)        # is it a list (ls.)
is.data.frame(one.dim.space)  # is it a dataframe (df.)

# Now that we know it is a vector, what kind of vector is it?

is.logical(one.dim.space)    # is it a logical (boolean) vector (vb.)
is.character(one.dim.space)  # is it a character vector (vc.)
is.factor(one.dim.space)     # is it a factor vector (vf.)
is.numeric(one.dim.space)    # is it a numeric vector (vn.)
is.integer(one.dim.space)    # is it an integer vector (vi.)

# R has some special commands to help inspect objects

class(one.dim.space)  # output will depend on datatype of object
str(one.dim.space)    # str for structure, very helpful summary of an object

# Now that we know one.dim.space is an vector of integers, let's prefix it
# appropriately

vi.one.dim.space <- one.dim.space # Note that one.dim.space still exists in our
                                  # environment. We created a new variable.

# Let's look at some functions that will help us to describe our one-dimensional 
# space.

dim(one.dim.space)  # Returns the number of rows and columns (dimensions) of a 
                    # matrix. Returns NULL since one.dim.space is a single-row
                    # vector, not a matrix.
length(one.dim.space)
unique(one.dim.space)
vi.unique.val <- unique(one.dim.space)   # We have assigned the list of unique 
                                         # values to the variable vi.unique.val

# Note when we do this that the newly defined variable appears in the
# environment window in the upper right

# We had briefly mentioned that one of the powerful things about a programming
# language like R is the ability to embed functions within functions

length(unique(one.dim.space))

# is equivalent to

length(
  unique(
    one.dim.space
    )
  )

# is equivalent to

length(vi.unique.val)

# Here are some more ways to describe our one-dimensional space

min(one.dim.space)
max(one.dim.space)
range(one.dim.space)

# If the range of our one-dimensional space was really large, we could use the 
# following:

head(one.dim.space) # By default, first six. Take a look at head in Help tab.
                    # Six is actually a default value and does not need to be
                    # stated explicitly
head(
  x = one.dim.space,
  n = 3
  ) # We will see more of this format going forward. I think it is good for
    # and easier to read by beginners. It also helps to learn the arguments of
    # the functions.

tail(one.dim.space)

# Let's look at some measures of variability

var(one.dim.space)  # variance
sd(one.dim.space)   # standard deviation

# Let's look at some measures of central tendency

mean(one.dim.space)
median(one.dim.space)
mode(one.dim.space)  # the function mode() in R is not the mathematical mode

# So let's create a function that will calculate the mathematical mode for us

# Syntax for a generic function in R is:
#  FunctionName <- function(arg1, arg2) {
#  <code that defines function>
#  <code that returns object>
#  }
# Unless otherwise specified, functions always return the last "action"

# One of the most powerful features of functions is the ability to embed
# functions within each other. When we execute the code, we create a Function
# object in R, which we can see in the Environment tab.

FindMode <- function(x) {
  uniqx <- unique(x)  # note embedded unique() function
  uniqx[which.max(tabulate(match(x, uniqx)))]  # don't worry about understanding 
                                               # what is happening here, for now 
                                               # it is just black box magic
}

# Although if you want to try to figure it out, a common way to do so is to run 
# each piece in reverse order of nesting

# 1
unique(one.dim.space)

# 2
match(one.dim.space, unique(one.dim.space))

# 3
tabulate(match(one.dim.space, unique(one.dim.space)))

# 4
which.max(tabulate(match(one.dim.space, unique(one.dim.space))))

# 5
unique(
  one.dim.space)[which.max(
    tabulate(
      match(
        one.dim.space, 
        unique(one.dim.space)
        )  # I prefer this format for its readabity and ease of de-bugging
      )
    )
    ]

# And we can see that our user-defined function returns the same...

FindMode(one.dim.space)

# Some other useful built-in summary functions:

summary(one.dim.space)
fivenum(one.dim.space)  # Tukey's Five Number Summary (min, lower-hinge, median,
                        # upper-hinge, max)
table(one.dim.space)  # ordered values with counts by value

# But, R does and doesn't do everything (this isn't a contradiction!)
 
# Just like R doesn't have a "built-in" FindMode function, R doesn't have 
# built-in skewness or kurtosis. But R does have lots and lots of user 
# contributed packages which have these, and many other functions.

install.packages('psych')  # loads package from CRAN website/mirror

# note that it also installed a dependency - the package mnormt which psych 
# requires

library(psych)  # loads package into memory (see Packages in lower right)

describe(one.dim.space)

# Let's visualize our one dimensional space

plot(one.dim.space)  # Not particularly helpful, note that R included an index, 
                     # rather than plotting on a straight line

one.dim.space[order(one.dim.space)]  # orders the values in the vector

plot(one.dim.space[order(one.dim.space)])  # a little better

# But what we really want is a histogram

hist(one.dim.space)
hist(
  x = one.dim.space,  # So far we've been lazy about our arguments, this is 
                      # how to specify them unambiguously.
  breaks = c(1:12) # Breaks is a vector specifying the number of bins+1 in
                   # the histogram.  
  )    

hist(
  x = one.dim.space,
  breaks = c(1:max(one.dim.space)) # again note how we can embed functions 
                                   # within functions within arguments
  )  

rm(list=ls())  # Clears global environment of all visible objects (some hidden
               # objects may remain)

# One final quirk for this lesson, let's do a Save As from the drop down.

# Note the warning when we try to save! You actually have to specify .r at the
# end of your filename. I forget this all the time - so annoying!

# END


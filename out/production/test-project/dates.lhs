
Problem:
-------

(a) Define a type Date that contains a day of the month, a month, and an
year, in this order. For example, should the constructor of that type be
Day, the following declaration should be valid: 

aDate :: Date
aDate =  Day 14 Feb 2001

Make your type an instance of Eq, Ord, and Show, either by implicit
derivation of by explicit instantiation. However, a date should be
displayed as follows: 

Main> Day 14 Apr 2001
April 14, 2001
Main>

Specifically, the representation of a date should contain the full name
of the month, followed by the day of the month, followed by a comma and
the year.

Furthermore, the comparison of two dates should return a logical
result. For example, 

Main> Day 14 Apr 2002 <= Day 15 Apr 2001
False
Main> 

even if the implicit comparison on tuples yields a different result:

Main> (14, Apr, 2002) <= (15, Apr, 2001)
True
Main> 


(b) Write a function succDate :: Date -> Date that receives a date and
returns its successor.  For example, the successor of December 31,
2000 is January 1, 2001. That is, your function should behave as
follows:

Main> succDate (Day 31 Dec 2000)
January 1, 2001
Main> succDate (Day 29 Feb 2000)
March 1, 2000
Main> succDate (Day 25 Feb 2000)
February 26, 2000
Main> 

Solution:
--------

(a) Let's define a type for months:

> data Month = Jan | Feb | Mar | Apr | May | Jun | 
>              Jul | Aug | Sep | Oct | Nov | Dec
>      deriving (Eq, Ord)

Now, the show function would display the whole name of the month. This
will spare me of some headaches later. 

> instance Show Month where
>   show Jan = "January"
>   show Feb = "February"
>   show Mar = "March"
>   show Apr = "April"
>   show May = "May"
>   show Jun = "June"
>   show Jul = "July"
>   show Aug = "August"
>   show Sep = "September"
>   show Oct = "October"
>   show Nov = "November"
>   show Dec = "December"

Then, the type Date might be:

> data Date = Day Int Month Int
>   deriving (Eq)

(the equality between two dates is naturally defined, hence I can derive
the class Eq)

the comparison is a little different from the standard (in fact, quite
the reverse of the standard...):

> instance Ord Date where 
>   Day d1 m1 y1 <= Day d2 m2 y2 = (y1, m1, d1) <= (y2, m2, d2)

As well, the show function is different from the standard:

> instance Show Date where 
>   show (Day d m y) = (show m) ++ " " ++ (show d) ++ ", " ++ (show y) 


(b) To make my life easier, I will make Month an instance of Enum:

> instance Enum Month where 
>   fromEnum Jan = 0
>   fromEnum Feb = 1
>   fromEnum Mar = 2
>   fromEnum Apr = 3
>   fromEnum May = 4
>   fromEnum Jun = 5
>   fromEnum Jul = 6
>   fromEnum Aug = 7
>   fromEnum Sep = 8
>   fromEnum Oct = 9
>   fromEnum Nov = 10
>   fromEnum Dec = 11
>   toEnum 0     = Jan
>   toEnum 1     = Feb
>   toEnum 2     = Mar
>   toEnum 3     = Apr
>   toEnum 4     = May
>   toEnum 5     = Jun
>   toEnum 6     = Jul
>   toEnum 7     = Aug
>   toEnum 8     = Sep
>   toEnum 9     = Oct
>   toEnum 10    = Nov
>   toEnum 11    = Dec
>   succ m       = toEnum (mod (fromEnum m + 1) 12)

Then, the maximum number of days in each month is given by 

> maxDay     :: Int -> Month -> Int
> maxDay y m =  maxdays !! (fromEnum m)
>   where maxdays  = [31, if (isLeap y) then 29 else 28, 
>                     31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
>         isLeap x | mod x 400 == 0 = True
>                  | mod x 100 == 0 = False
>                  | mod x 4   == 0 = True
>                  | otherwise      = False

Alternative, more compact definition for isLeap:

   isLeap x = (mod x 4 == 0) && ((mod x 100) /= 0 || (mod y 400) == 0)

Now, one can define an instance of Enum for the type Date: 

> instance Enum Date where 
>   succ (Day d m y)
>     | d >= maxDay y m && m == Dec = Day 1 (succ m) (y+1)
>     | d >= maxDay y m             = Day 1 (succ m) y
>     | otherwise                   = Day (succ d) m y

Finally, the function succDate is

> succDate :: Date -> Date
> succDate =  succ

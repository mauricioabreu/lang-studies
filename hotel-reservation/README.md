# Hotel Reservation

A hotel chain operating in Miami wishes to offer room reservation services over the internet. They have three hotels in Miami: Lakewood, Bridgewood and Ridgewood. Each hotel has separate weekday and weekend(Saturday and Sunday) rates. There are special rates for rewards customer as a part of loyalty program. Each hotel has a rating assigned to it.

**Lakewood** with a rating of 3 has weekday rates as 110$ for regular customer and 80$ for rewards customer. The weekend rates are 90$ for regular customer and 80$ for a rewards customer.

**Bridgewood** with a rating of 4 has weekday rates as 160$ for regular customer and 110$ for rewards customer. The weekend rates are 60$ for regular customer and 50$ for a rewards customer.

**Ridgewood** with a rating of 5 has weekday rates as 220$ for regular customer and 100$ for rewards customer. The weekend rates are 150$ for regular customer and 40$ for a rewards customer.
Write a program to help an online customer find the cheapest hotel.

The input to the program will be a range of dates for a regular or rewards customer. The output should be the cheapest available hotel. In case of a tie, the hotel with highest rating should be returned.

**INPUT FORMAT:** <customer_type>: <date1>, <date2>, <date3>, ...

**OUTPUT FORMAT:** <name_of_the_cheapest_hotel>

**INPUT 1:** Regular: 16Mar2009(mon), 17Mar2009(tues), 18Mar2009(wed)

**OUTPUT 1:** Lakewoods

## Running

There are two ways to run this program: passing the input as arguments or passig a file with all inputs.

Using args:

```
runhaskell Main.hs "Regular: 20Mar2009(fri), 21Mar2009(sat), 22Mar2009(sun)"

Hotel Reservation
Just Bridgewood
```

Using a file:

```
runhaskell Main.hs -f "inputs.txt"

Hotel Reservation
Just Lakewood
Just Bridgewood
Just Lakewood
Nothing
Nothing
```


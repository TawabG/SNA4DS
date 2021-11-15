import time

if __name__=='__main__':
    # User variables
    output_file_name = "reviews_winter_2001_2002.csv"
    input_file_name = 'reviews_half_2000_to_half_2002.csv'

    lowerbound_date = time.strptime('2001-12-21', '%Y-%m-%d')
    upperbound_date = time.strptime('2002-03-20', '%Y-%m-%d')


    # function
    with open(output_file_name, "w") as outputfile:
        outputfile.write(",CustomerID,Rating,Date,Movie_Id\n")
        for line in open(input_file_name):
            line_list = line.split(',')
            date_string = line_list[3]
            # if it is the first line
            if(date_string == 'Date'):
                continue
            date = time.strptime(date_string, '%Y-%m-%d')

            if(date > lowerbound_date):
                outputfile.write(','.join(line_list))
            # stop when date is higher than upperbound since input file is sorted on date
            if(date >= upperbound_date):
                break
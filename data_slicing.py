import pandas as pd
import numpy as np

def load_ratings(files):
    ratings_df = pd.DataFrame(columns=['CustomerID','Rating', 'Date'])
    for file_location in files:
        partial_ratings = pd.read_csv(file_location, header = None, names = ['CustomerID','Rating', 'Date'], usecols = [0,1, 2])
        partial_ratings['Rating'] = partial_ratings['Rating'].astype(float)
        ratings_df = ratings_df.append(partial_ratings)

    ratings_df = ratings_df.reset_index(drop=True)
    return ratings_df

def map_user_ratings_to_movies(ratings_df):
    # This function takes 1,5 hours with the fulll dataset on my computer
    df_nan = pd.DataFrame(pd.isnull(ratings_df.Rating))
    df_nan = df_nan[df_nan['Rating'] == True]
    df_nan = df_nan.reset_index()

    movie_np = []
    movie_id = 1

    for i,j in zip(df_nan['index'][1:],df_nan['index'][:-1]):

        temp = np.full((1,i-j-1), movie_id)
        movie_np = np.append(movie_np, temp)
        movie_id += 1

    last_record = np.full((1,len(ratings_df) - df_nan.iloc[-1, 0] - 1),movie_id)
    movie_np = np.append(movie_np, last_record)

    df = ratings_df[pd.notnull(ratings_df['Rating'])]

    df['Movie_Id'] = movie_np.astype(int)
    df['CustomerID'] = df['CustomerID'].astype(int)
    return df



def trim_ratings_data_by_percentile(df, movies_percentile=0.7, users_percentile=0.7):
    df_movie_summary = df.groupby('Movie_Id')['Rating'].agg(['count'])
    df_movie_summary.index = df_movie_summary.index.map(int)
    movie_benchmark = round(df_movie_summary['count'].quantile(movies_percentile),0)
    drop_movie_list = df_movie_summary[df_movie_summary['count'] < movie_benchmark].index

    df_cust_summary = df.groupby('CustomerID')['Rating'].agg(['count'])
    df_cust_summary.index = df_cust_summary.index.map(int)
    cust_benchmark = round(df_cust_summary['count'].quantile(users_percentile),0)
    drop_cust_list = df_cust_summary[df_cust_summary['count'] < cust_benchmark].index

    print('Movies minimum rating count: {}'.format(movie_benchmark))
    print('Customers minimum rating count: {}'.format(cust_benchmark))

    print('Original Shape: {}'.format(df.shape))
    df = df[~df['Movie_Id'].isin(drop_movie_list)]
    df = df[~df['CustomerID'].isin(drop_cust_list)]

    print('After Trim Shape: {}'.format(df.shape))


if __name__ == '__main__':
    files = ['../combined_data_1.txt', '../combined_data_2.txt', '../combined_data_3.txt', '../combined_data_4.txt']
    ratings_df = load_ratings(files)

    # realiseer je dat het uitvoeren van deze functie 6-8 GB aan werkgeheugen kost en over 100.000.000 ratings loopt dus lang duurt
    map_user_ratings_to_movies(ratings_df)
    

    print(ratings_df.head())



    
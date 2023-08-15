import psycopg2
import argparse
import socket
import time

# Define command line arguments
parser = argparse.ArgumentParser()
parser.add_argument("--port", type=int, help="PostgreSQL port number")
args = parser.parse_args()

# PostgreSQL connection parameters
DB_HOST = '192.168.110.174'
DB_PORT = args.port
DB_NAME = 'postgres'
DB_USER = 'postgres'
DB_PASSWORD = 'postgres'

# Number of times to attempt connection
MAX_ATTEMPTS = 10

def connect():
    """Attempts to connect to the PostgreSQL database"""
    attempts = 0
    while attempts < MAX_ATTEMPTS:
        try:
            conn = psycopg2.connect(
                host=DB_HOST,
                port=DB_PORT,
                dbname=DB_NAME,
                user=DB_USER,
                password=DB_PASSWORD
            )
            # Print the output of SELECT pg_is_in_recovery(),inet_server_addr() after successfully connecting
            cursor = conn.cursor()
            cursor.execute("SELECT pg_is_in_recovery(),inet_server_addr()")
            result = cursor.fetchone()
            print(f"Connected to: {result[1]}")
            return conn
        except psycopg2.OperationalError:
            attempts += 1
            print(f"Connection attempt {attempts} failed. Retrying in 1 seconds...")
            time.sleep(1)
    raise Exception("Failed to connect to the PostgreSQL database after multiple attempts.")

def perform_query():
    """Performs a query based on the recovery mode of the PostgreSQL server"""
    conn = connect()
    cursor = conn.cursor()

    # Execute the appropriate SQL statement based on the recovery mode of the PostgreSQL server
    cursor.execute("SELECT pg_is_in_recovery()")
    is_in_recovery = cursor.fetchone()[0]
    if is_in_recovery:
        print("PostgreSQL server is in recovery mode. Performing SELECT query...")
        cursor.execute("SELECT COUNT(1) FROM emp")
        result = cursor.fetchone()[0]
        print(f"Count: {result}")
    else:
        print("PostgreSQL server is not in recovery mode. Performing INSERT query...")
        cursor.execute("INSERT INTO emp (id, sal) VALUES (%s, %s)", ('1', '1'))
        conn.commit()
        print("Data inserted successfully")
        cursor.execute("SELECT COUNT(1) FROM emp")
        result = cursor.fetchone()[0]
        print(f"Count: {result}")


    # Close the database connection
    conn.close()

# Loop indefinitely and perform a query based on the recovery mode of the PostgreSQL server
while True:
    try:
        perform_query()
        print("Press Ctrl+Z to terminating the job...")
        time.sleep(1)
        print()
    except:
        print("Error performing query. Retrying in 1 second...")
        time.sleep(1)

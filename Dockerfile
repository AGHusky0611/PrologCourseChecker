# Use the official SWI-Prolog Docker image
FROM swipl:stable

# Set the working directory inside the container
WORKDIR /app

# Copy the Prolog source files and the static directory to the container
COPY courses.pl .
COPY server.pl .
COPY static/ ./static/

# Expose the port the server will run on
# Render will set the PORT environment variable, which our server will use
EXPOSE 8080

# Command to run the Prolog server
# This will execute the 'start' predicate in server.pl
CMD ["swipl", "-s", "server.pl", "-g", "start", "-t", "halt"]

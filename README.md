# Build the docker image

In a terminal run: 

```
> docker build -t tool .
```

# Run the container

```
> docker run -d -p 80:80 tool
```

# Use the tool 

Open a browser and go to `localhost:80`

# Observation 

If you change the source code and you want to rebuild the image and run the new container, please,
notice that some browsers might required to clear the cache in order to properly show the
new version of the tool. 
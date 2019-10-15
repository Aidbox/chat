FROM clojure:tools-deps as builder

RUN mkdir /app
WORKDIR /app
ADD . /app

# Build
RUN clojure -A:uberjar

#RUN zip -d /app/app.jar 'META-INF/*.SF' 'META-INF/*.RSA' 'META-INF/*SF'

FROM java:8
EXPOSE 8080

ENV PORT="8080"

COPY --from=builder /app/app.jar/ /app.jar
RUN mkdir -p /resources/html
ADD resources/html/* /resources/html/
RUn mkdir -p /data
CMD java -jar /app.jar

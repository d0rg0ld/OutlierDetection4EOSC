FROM rocker/shiny:3.5.1

RUN apt-get update -y
RUN apt-get install -y python3-pip r-cran-littler r-cran-optparse libssl-dev

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

COPY requirements.txt /usr/src/app/

RUN pip3 install --no-cache-dir -r requirements.txt

COPY . /usr/src/app

EXPOSE 8080

ENTRYPOINT ["python3"]

CMD ["-m", "swagger_server"]

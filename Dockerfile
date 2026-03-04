FROM ubuntu:24.04
EXPOSE 3000
RUN apt-get update && apt-get install -y ca-certificates
COPY ./zpl-printer /bin/zpl-printer
CMD ["/bin/zpl-printer", "serve"]

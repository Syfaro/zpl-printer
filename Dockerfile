FROM ubuntu:22.04
EXPOSE 3000
COPY ./zpl-printer /bin/zpl-printer
CMD ["/bin/zpl-printer"]

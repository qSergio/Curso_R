library(sparklyr)

spark_install()

sc <- spark_connect(master = "local")
spark_disconnect(sc)

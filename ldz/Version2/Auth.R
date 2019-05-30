library(rdrop2)
token <- drop_auth()
saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
#https://github.com/karthik/rdrop2#accessing-dropbox-on-shiny-and-remote-servers
# read it back with readRDS

# Delete file if it exists
if(file.exists(list.files(pattern = "asreml"))) {
    i <- 1
    while(file.exists(list.files(pattern = "asreml"))) {
        file.remove(list.files(pattern = "asreml")[i])
        i <- i + 1
    }
}


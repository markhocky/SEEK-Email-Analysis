
# Analysis of downloaded SEEK emails
# View numbers of job types (based on advert heading)
# View typical employment type (contract etc) for job type
# View salary informaction for job type


title.split.chars <- c("[|]", "-", " x ", "[(]")

trim <- function(x) {
    gsub("^\\s+|\\s+$", "", x)
}

read.line <- function(line) {

    line <- trim(line)
    content <- trim(strsplit(line, ":[^/]")[[1]])
    return(content)
}


read.section <- function(lines, line.no) {

    line <- read.line(lines[line.no])
    last.entry <- line[length(line)]
    last.character <- substr(last.entry, nchar(last.entry), nchar(last.entry))
    if (last.character == "=") {
        next.line <- read.section(lines, line.no + 1)
        last.entry <- sub("=", next.line$entry, last.entry)
        line.no <- next.line$line.no
    }

    line[length(line)] <- last.entry

    return(list(entry = line, line.no = line.no))
}


read.job <- function(lines, line.no) {

    results = list()

    while (lines[line.no] != "") {

        
        section <- read.section(lines, line.no)
        section.heading <- section$entry[1]
        results[[section.heading]] <- section$entry[-1]

        line.no <- section$line.no + 1
    }

    return(results)
}

make.DF.row <- function(job) {

    full.title <- job[[1]]
    job.title <- trim(strsplit(full.title, title.split.chars)[[1]][1])
    title.pieces <- strsplit(job.title, "[^[:alpha:]]")[[1]]
    title.pieces <- title.pieces[title.pieces != ""]
    end <- length(title.pieces)

    return(data.frame(title = job.title,
                      role = title.pieces[end], 
                      type = paste(title.pieces[-end], collapse = " "),
                      label = title.pieces[1],
                      advertiser = job$Advertiser,
                      salary = ifelse(is.null(job$Salary), NA, job$Salary)))
}

data.folder <- "C:/Users/Mark/Documents/Visual Studio 2015/Projects/HotmailRetrieval/Data/"
emails <- list.files(data.folder)

jobs <- data.frame()

for (email in emails) {
    lines <- readLines(paste0(data.folder, email))
    job.number <- 1
    while (TRUE) {

        job.start.line <- grep(paste0("Job ", job.number, ":"), lines)
        if (!length(job.start.line)) {
            break
        }

        job <- read.job(lines, job.start.line)
        jobs <- rbind(jobs, make.DF.row(job))
        job.number <- job.number + 1
    }
}


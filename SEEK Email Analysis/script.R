
# Analysis of downloaded SEEK emails
# View numbers of job types (based on advert heading)
# View typical employment type (contract etc) for job type
# View salary informaction for job type


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
        last.entry <- sub("=", next.line$entry[1], last.entry)
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
    job.title <- trim(strsplit(full.title, ".[-|(]| x ")[[1]][1])
    starting.brackets <- grepl("^\\(.*\\)", job.title)
    if (starting.brackets) {
        job.title <- trim(strsplit(job.title, "[)]")[[1]][-1])
    }
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

collate.jobs <- function(data.folder) {
    emails <- list.files(data.folder)
    jobs <- data.frame()

    for (email in emails) {
        lines <- readLines(paste0(data.folder, email))
        job.lines <- grep("Job [[:digit:]]", lines)
        for (job.start.line in job.lines) {
            job <- read.job(lines, job.start.line)
            jobs <- rbind(jobs, make.DF.row(job))
        }
    }

    return(jobs)
}

data.folder <- "C:/Users/Mark/Documents/Visual Studio 2015/Projects/HotmailRetrieval/Data/"
jobs <- collate.jobs(data.folder)

most.common <- function(type, jobs, number = 10) {
    types <- sort(summary(jobs[[type]]), decreasing = TRUE)
    types <- types[names(types) != "(Other)"]
    types <- types[names(types) != "NA's"]
    dotchart(types[number:1])
    title(paste0("Top ", number, " ", type, "s"), xlab = "Number advertised")
}




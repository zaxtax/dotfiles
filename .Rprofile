# set a CRAN mirror
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.stat.ucla.edu"
options(repos=r)})

.Last <- function() {
        if (!any(commandArgs()=='--no-readline') && interactive()){
                require(utils)
                try(savehistory(Sys.getenv("R_HISTFILE")))
        }
}

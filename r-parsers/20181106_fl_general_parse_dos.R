library(tidyverse)
library(readxl)
library(httr)

#######################################################################################
# SET USER VARIABLES BELOW
#######################################################################################
cfilepath <- "r-parsers/20181106_fl_general_defs.csv"
zindir  <- "r-parsers/input2018/" # full path or path relative to working directory
zoutdir <- "C:/Users/bdavi/OneDrive2/github/oe/openelections-data-fl/r-parsers/outdos2018/" # full path or path relative to working directory
filestate <- "FL"
#######################################################################################
# END OF USER VARIABLES
#######################################################################################

fl_county_codes <- c(
    "ALA","BAK","BAY","BRA","BRE",
    "BRO","CAL","CHA","CIT","CLA",
    "CLL","CLM","DAD","DES","DIX",
    "DUV","ESC","FLA","FRA","GAD",
    "GIL","GLA","GUL","HAM","HAR",
    "HEN","HER","HIG","HIL","HOL",
    "IND","JAC","JEF","LAF","LAK",
    "LEE","LEO","LEV","LIB","MAD",
    "MAN","MON","MRN","MRT","NAS",
    "OKA","OKE","ORA","OSC","PAL",
    "PAS","PIN","POL","PUT","SAN",
    "SAR","SEM","STJ","STL","SUM",
    "SUW","TAY","UNI","VOL","WAK",
    "WAL","WAS")
fl_counties <- c(
    "Alachua","Baker","Bay","Bradford","Brevard",
    "Broward","Calhoun","Charlotte","Citrus","Clay",
    "Collier","Columbia","Miami-Dade","Desoto","Dixie",
    "Duval","Escambia","Flagler","Franklin","Gadsden",
    "Gilchrist","Glades","Gulf","Hamilton","Hardee",
    "Hendry","Hernando","Highlands","Hillsborough","Holmes",
    "Indian River","Jackson","Jefferson","Lafayette","Lake",
    "Lee","Leon","Levy","Liberty","Madison",
    "Manatee","Monroe","Marion","Martin","Nassau",
    "Okaloosa","Okeechobee","Orange","Osceola","Palm Beach",
    "Pasco","Pinellas","Polk","Putnam","Santa Rosa",
    "Sarasota","Seminole","St. Johns","St. Lucie","Sumter",
    "Suwannee","Taylor","Union","Volusia","Wakulla",
    "Walton","Washington")

zstate  <- NULL
zcounty <- NULL

errmsg <- function(msg){
    logfile <- paste0(zoutdir,"logfile.txt")
    print(paste0(zstate,"|",zcounty,"|##### ",msg))
    cat(paste0(zstate,"|",zcounty,"|##### ",msg,"\n"), file = logfile, append = TRUE)
}
warnmsg <- function(msg){
    logfile <- paste0(zoutdir,"logfile.txt")
    print(paste0(zstate,"|",zcounty,"|===== ",msg))
    cat(paste0(zstate,"|",zcounty,"|===== ",msg,"\n"), file = logfile, append = TRUE)
}
infomsg <- function(msg){
    logfile <- paste0(zoutdir,"logfile.txt")
    print(paste0(zstate,"|",zcounty,"|..... ",msg))
    cat(paste0(zstate,"|",zcounty,"|..... ",msg,"\n"), file = logfile, append = TRUE)
}

parsefile <- function(county, state, xx){
    # xx <- read_csv(cfile)
    # infile <- paste0(zindir,"20181106_",tolower(state),"_",tolower(county),".csv")
    # write_csv(xx, infile)
    got_ushouse <- FALSE
    got_statehouse <- FALSE
    got_statesenate <- FALSE
    ushouse_nn <- NULL
    statehouse_nn <- NULL
    statesenate_nn <- NULL
    xvotes <- FALSE
    # Ensure all columns are present
    xx$'Provisional Votes' <- 0
    xx$'Mail Votes' <- 0
    xx$'Early Votes' <- 0
    xx$'Election Day Votes' <- 0
    xx$absentee <- 0
    xx$limited <- 0
    xx$county <- county
    #xx$district <- NA
    dd <- data.frame(xx$COUNTY,xx$AREA,xx$Contest,xx$DIST,xx$Party,xx$Candidate,
                     xx$Votes,xx$`Early Votes`,xx$`Election Day Votes`,
                     xx$`Provisional Votes`,xx$`Mail Votes`,xx$absentee,xx$limited,
                     stringsAsFactors = FALSE)
    names(dd) <- c("county","precinct","office","district","party","candidate",
                   "votes","early_voting","election_day","provisional","mail",
                   "absentee","limited")
    dd[is.na(dd)] = ""
    xx <- dd
    gotxx <- TRUE
    
    #######################################################################################
    # The following code attempts to clean the data for all counties and was derived from
    # https://github.com/openelections/openelections-data-tx/blob/master/r-parsers/20201103_tx_general_parse.R .
    # If needed, the code can be made faster by wrapping any sections of code that are
    # unlikely to apply to many counties in if statements so that they are county-specific. 
    #######################################################################################
    if (gotxx){
        gxx1 <<- xx #DEBUG-RM
        adj_county <- gsub("[\\. -]+","_",tolower(county)) # replace any group of [. -] with _
        f_std <- paste0("20181106__",tolower(state),"__general__",adj_county,"__precinct.csv")
        file_std <- paste0(zoutdir,f_std)
        # Changes to match standard
        xx$district <- trimws(xx$district)
        xx$district <- gsub("^District ","",xx$district, ignore.case = TRUE)
        xx$precinct <- gsub("^Precinct [0]*","",xx$precinct, ignore.case = TRUE)
        if (toupper(county) == "COLLIN"){
            xx$precinct <- gsub("^PCT [0]*","",xx$precinct, ignore.case = TRUE)
        }
        #xx$precinct <- gsub("^PCT ","",xx$precinct, ignore.case = TRUE) # Wharton County
        xx$precinct <- str_squish(xx$precinct) # squish multiple to single space - Dixie, Columbia, Hernando County,FL
        xx$party <- trimws(xx$party)
        xx$party[xx$party == "(D)"] <- "DEM" # El Paso County
        xx$party[xx$party == "(R)"] <- "REP" # El Paso County
        xx$party[xx$party == "(G)"] <- "GRN" # El Paso County
        xx$party[xx$party == "(I)"] <- "IND" # El Paso County
        xx$party[xx$party == "(L)"] <- "LIB" # El Paso County
        xx$party[grepl("^Democratic",xx$party, ignore.case = TRUE)] <- "DEM" #Brazos County
        xx$party[grepl("^Republican",xx$party, ignore.case = TRUE)] <- "REP" #Brazos County
        xx$candidate <- str_squish(xx$candidate) # change all multiple spaces to a single space - Ellis County
        xx$office <- str_squish(xx$office) # change all multiple spaces to a single space
        xx$office <- str_to_title(xx$office)
        xx$office[xx$office == "Registered Voters - Total"] <- "Registered Voters"
        xx$candidate[grepl("^REGISTERED VOTERS",xx$candidate,ignore.case = TRUE)] <- ""
        xx$county <- str_to_title(xx$county) #Cooke County
        
        xx$office[grepl("^Governor and L",xx$office,ignore.case = TRUE)] <- "Governor" # standardize by deleting Lt. Governor
        xx$office[grepl("^Governor & L",xx$office,ignore.case = TRUE)] <- "Governor"   # standardize by deleting Lt. Governor
        xx$office[grepl("^Governor/L",xx$office,ignore.case = TRUE)] <- "Governor"     # standardize - Levy County,FL
        xx$office[grepl("^Gov./L",xx$office,ignore.case = TRUE)] <- "Governor"         # standardize - Santa Rosa County,FL
        #xx$office[xx$office == "Ballots Cast - Blank"] <- "BALLOTS CAST - BLANK" # avoids next statement
        #xx$office[grepl("^Ballots Cast -",xx$office)] <- "Ballots Cast"
        xx$office[grepl("^Ballots Cast - Republican",xx$office)] <- "Ballots Cast"
        xx$office[grepl("^Ballots Cast - Democratic",xx$office)] <- "Ballots Cast"
        xx$office[grepl("^Ballots Cast - Nonpartisan",xx$office)] <- "Ballots Cast"
        xx$office[grepl("^Ballots Cast - Total",xx$office)] <- "Ballots Cast"
        xx$candidate[grepl("^BALLOTS CAST",xx$candidate,ignore.case = TRUE)] <- ""
        xx$party[grepl("^\\(R\\) ",xx$office, ignore.case = TRUE)] <- "REP" # El Paso County
        xx$party[grepl("^\\(D\\) ",xx$office, ignore.case = TRUE)] <- "DEM" # El Paso County
        xx$party[grepl("^Rep ",xx$office, ignore.case = TRUE)] <- "REP"
        xx$party[grepl("^Dem ",xx$office, ignore.case = TRUE)] <- "DEM"
        xx$party[grepl("^Lib ",xx$office, ignore.case = TRUE)] <- "LIB" # Cooke County
        xx$party[grepl("^Grn ",xx$office, ignore.case = TRUE)] <- "GRN" # Cooke County
        xx$party[grepl("^DEM ",xx$candidate, ignore.case = TRUE)] <- "DEM" # Dallas County
        xx$party[grepl("^REP ",xx$candidate, ignore.case = TRUE)] <- "REP" # Dallas County
        xx$party[grepl("^GRN ",xx$candidate, ignore.case = TRUE)] <- "GRN" # Dallas County
        xx$party[grepl("^IND ",xx$candidate, ignore.case = TRUE)] <- "IND" # Dallas County
        xx$party[grepl("^LIB ",xx$candidate, ignore.case = TRUE)] <- "LIB" # Dallas County
        xx$office <- gsub(" \\(R\\)$","",xx$office, ignore.case = TRUE) # Hays County
        xx$office <- gsub(" \\(D\\)$","",xx$office, ignore.case = TRUE) # Hays County
        xx$office <- gsub("^\\(R\\) ","",xx$office, ignore.case = TRUE) # El Paso County
        xx$office <- gsub("^\\(D\\) ","",xx$office, ignore.case = TRUE) # El Paso County
        xx$office <- gsub("^Rep\\.? ","",xx$office, ignore.case = TRUE)
        xx$office <- gsub("^Dem\\.? ","",xx$office, ignore.case = TRUE)
        xx$office <- gsub("^Lib\\.? ","",xx$office, ignore.case = TRUE) # Cooke County
        xx$office <- gsub("^Grn\\.? ","",xx$office, ignore.case = TRUE) # Cooke County
        xx$office <- gsub("^Rep ","",xx$office, ignore.case = TRUE) # delete double Rep
        xx$office <- gsub("^Dem ","",xx$office, ignore.case = TRUE) # delete double Dem
        xx$office <- gsub(" Of "," of ",xx$office)
        xx$office <- gsub(" At "," at ",xx$office)
        xx$office <- gsub("-At-","-at-",xx$office) # Austin County
        xx$office <- gsub(" The "," the ",xx$office) # Clay County
        xx$office <- gsub(" And "," and ",xx$office) # Goliad County
        xx$office <- gsub("^Jop ","JOP ",xx$office) # Donley County
        xx$office <- gsub("State Boe","State BoE",xx$office)
        xx$office <- gsub("President / Vice-President","President",xx$office) # Rusk County
        xx$office <- gsub("State Senator","State Senate",xx$office) # Clay County
        xx$office <- gsub("State Representative","State House",xx$office) # Callahan County
        xx$office <- gsub("State House of Representatives","State House",xx$office) # Atkinson County,GA
        xx$office <- gsub("State Rep\\.?","State House",xx$office) # Dallas County
        xx$office <- gsub("^U\\.? ?s\\.? Senator","U.S. Senate",xx$office, ignore.case = TRUE) # Rusk County
        xx$office <- gsub("^U\\.? ?s\\.? Representative","U.S. House",xx$office, ignore.case = TRUE) # Brazos County
        xx$office <- gsub("^U\\.? ?s\\.? House of Representatives","U.S. House",xx$office, ignore.case = TRUE) # Appling County,GA
        xx$office <- gsub("^U\\.? ?s\\.? Rep,","U.S. House,",xx$office, ignore.case = TRUE) # Goliad County
        xx$office <- gsub("^United States Representative","U.S. House",xx$office, ignore.case = TRUE) # Guadalupe County
        xx$office <- gsub("^U\\.s\\. Congressional","U.S. House",xx$office, ignore.case = TRUE) # Dallas County
        xx$office <- gsub("^Representative in Congress District \\d+ Congress","U.S. House",xx$office, ignore.case = TRUE) # Volusia County,FL
        xx$office <- gsub("^Representative In Congress","U.S. House",xx$office, ignore.case = TRUE) # Alachua County,FL
        xx$office <- gsub("^U.S. House In Congress","U.S. House",xx$office, ignore.case = TRUE) # Duval County,FL
        xx$office <- gsub("^Representative Congress","U.S. House",xx$office, ignore.case = TRUE) # Miami-Dade County,FL
        xx$office <- gsub("^United States Senator","U.S. Senate",xx$office, ignore.case = TRUE) # Miami-Dade County,FL
        xx$office <- gsub("^United State Senate","U.S. Senate",xx$office, ignore.case = TRUE) # Manatee County,FL
        xx$office <- gsub("^In Congress","U.S. House",xx$office, ignore.case = TRUE) # Santa Rosa County,FL
        xx$office <- gsub("^U.S. House 17","U.S. House, Dist 17",xx$office, ignore.case = TRUE) # Sarasota County,FL
        xx$office <- gsub("^State House 84","State House, Dist 84",xx$office, ignore.case = TRUE) # St. Lucie County,FL
        xx$office <- gsub("^State House 85","State House, Dist 85",xx$office, ignore.case = TRUE) # St. Lucie County,FL
        xx$candidate <- gsub("^DEM ","",xx$candidate, ignore.case = TRUE) # Rusk County
        xx$candidate <- gsub("^REP ","",xx$candidate, ignore.case = TRUE) # Rusk County
        xx$candidate <- gsub("^LIB ","",xx$candidate, ignore.case = TRUE) # Rusk County
        xx$candidate <- gsub("^GRN ","",xx$candidate, ignore.case = TRUE) # Rusk County
        xx$candidate <- gsub("^\\(D\\)","",xx$candidate, ignore.case = TRUE) # El Paso County
        xx$candidate <- gsub("^\\(R\\)","",xx$candidate, ignore.case = TRUE) # El Paso County
        xx$candidate <- gsub("^\\(G\\)","",xx$candidate, ignore.case = TRUE) # El Paso County
        xx$candidate <- gsub("^\\(I\\)","",xx$candidate, ignore.case = TRUE) # El Paso County
        xx$candidate <- gsub("^\\(L\\)","",xx$candidate, ignore.case = TRUE) # El Paso County

        for (i in 1:NROW(xx)){
            # get district, if present
            #mm <- str_match(xx$office[i], "(.+)\\,? ?\\- ?Dist\\.?(?:rict)?(?: No.)? (\\d+)")
            mm <- str_match(xx$office[i], "(.+)\\,? ?\\-? ?Dist\\.?(?:rict)?(?: No.)? (\\d+)")
            if(!is.na(mm[1,1])){
                xx$district[i] <- mm[1,3]
                ndist <- as.numeric(mm[1,3])
                xxoffice <- trimws(mm[1,2])
                #xxoffice <- gsub("\\,$","",xxoffice, ignore.case = TRUE) # Brevard County,FL (remove trailing comma)
                xxoffice <- gsub("[\\, -]+$","",xxoffice, ignore.case = TRUE) # Brevard and Palm Beach County,FL (remove trailing comma)
                if (xxoffice == "U.S. House"){
                    got_ushouse <- TRUE
                    if (!(ndist %in% ushouse_nn)){
                        ushouse_nn <- c(ushouse_nn,ndist)
                    }
                }
                else if (xxoffice == "State House"){
                    got_statehouse <- TRUE
                    if (!(ndist %in% statehouse_nn)){
                        statehouse_nn <- c(statehouse_nn,ndist)
                    }
                }
                else if (xxoffice == "State Senate"){
                    got_statesenate <- TRUE
                    if (!(ndist %in% statesenate_nn)){
                        statesenate_nn <- c(statesenate_nn,ndist)
                    }
                }
                xx$office[i] <- xxoffice
            }
            #mm <- str_match(xx$office[i],"Us Rep(?: \\d+ Us)\\,? Dist\\.?(?:rict)?(?: No.)? (\\d+)") #DEBUG-CHECK
        }
        gxx <<- xx #DEBUG-RM
        
        xx$votes        <- as.numeric(xx$votes)
        if (xvotes){
            xx$absentee     <- as.numeric(xx$absentee)
            xx$early_voting <- as.numeric(xx$early_voting)
            xx$election_day <- as.numeric(xx$election_day)
            xx$mail         <- as.numeric(xx$mail)
            xx$provisional  <- as.numeric(xx$provisional)
            xx$limited      <- as.numeric(xx$limited)
        }
        
        # Only delete if all votes for all lines in an office group are zero
        #xx <- xx[xx$office != "Registered Voters - Nonpartisan",] # Robertson County
        lastoffice <- ""
        nonzero <- TRUE
        for (i in 1:NROW(xx)){
            if (xx$office[i] != lastoffice){
                if (is.na(nonzero) | !nonzero){
                    if (grepl("Registered Voter",xx$office[i]) | grepl("Ballots Cast",xx$office[i])){
                        for (j in firsti:(i-1)){ #mark for removal
                            xx$county[j] <- NA
                        }
                    }
                }
                if (xvotes){
                    nonzero <- (xx$votes[i] != 0 | xx$absentee[i] != 0 | xx$early_voting[i] != 0 | xx$election_day[i] != 0 | xx$mail[i] != 0 | xx$provisional[i] != 0 | xx$limited[i] != 0)
                }
                else{
                    nonzero <- (xx$votes[i] != 0)
                }
                lastoffice <- xx$office[i]
                firsti <- i
            }
            else{
                if (is.na(nonzero) | !nonzero){
                    if (xvotes){
                        nonzero <- (xx$votes[i] != 0 | xx$absentee[i] != 0 | xx$early_voting[i] != 0 | xx$election_day[i] != 0 | xx$mail[i] != 0 | xx$provisional[i] != 0 | xx$limited[i] != 0)
                    }
                    else{
                        nonzero <- (xx$votes[i] != 0)
                    }
                }
            }
        }
        xx <- xx[!is.na(xx$county),]
        
        xx$party[is.na(xx$party)] <- ""
        if (xvotes){
            # delete provisional if their sum = 0
            iprov <- which(names(xx) == "provisional")
            sumprov <- sum(xx$provisional, na.rm = TRUE)
            if (sumprov == 0){
                xx <- xx[-iprov]
            }
            # delete absentee if their sum = 0
            iabsent <- which(names(xx) == "absentee")
            sumabsent <- sum(xx$absentee, na.rm = TRUE)
            if (sumabsent == 0){
                xx <- xx[-iabsent]
            }
            # delete mail if their sum = 0
            imail <- which(names(xx) == "mail")
            summail <- sum(xx$mail, na.rm = TRUE)
            if (summail == 0){
                xx <- xx[-imail]
            }
            # delete limited if their sum = 0
            ilimited <- which(names(xx) == "limited")
            sumlimited <- sum(xx$limited, na.rm = TRUE)
            if (sumlimited == 0){
                xx <- xx[-ilimited]
            }
            if (toupper(county) == "CRANE"){
                ielection_day <- which(names(xx) == "election_day")
                sumelection_day <- sum(xx$election_day, na.rm = TRUE)
                if (sumelection_day == 0){
                    xx <- xx[-ielection_day]
                    iearly_voting <- which(names(xx) == "early_voting") # Crane County
                    xx <- xx[-iearly_voting]
                }
            }
        }
        else{
            xx <- xx[1:7] # include county,precinct,office,district,party,candidate,votes
        }
        # fix missing precincts if surrounded by same precinct - Austin County
        ipna <- which(is.na(xx$precinct))
        lpna <- length(ipna)
        if (lpna > 0){
            zipna <<- ipna #DEBUG
            zlpna <<- lpna #DEBUG
            for (i in 1:lpna){
                if (ipna[i] > 1 & ipna[i] < NROW(xx)){
                    if (xx$precinct[ipna[i]-1] == xx$precinct[ipna[i]+1]){
                        xx$precinct[ipna[i]] <- xx$precinct[ipna[i]-1]
                    }
                }
            }
        }
        zxx <<- xx #DEBUG
        write_csv(xx, file_std, na = "")
        infomsg(paste0(" AFTER write ", file_std))
    }
    # if (is.null(ushouse_nn)){
    #     warnmsg("Failed to find U.S. House")
    # }
    # else{
    #     infomsg(paste0("U.S. House   = ",paste0(ushouse_nn,collapse = ",")))
    # }
    # if (is.null(statehouse_nn)){
    #     warnmsg("Failed to find State House")
    # }
    # else{
    #     infomsg(paste0("State House  = ",paste0(statehouse_nn,collapse = ",")))
    # }
    # if (is.null(statesenate_nn)){
    #     warnmsg("Failed to find State Senate")
    # }
    # else{
    #     infomsg(paste0("State Senate = ",paste0(statesenate_nn,collapse = ",")))
    # }
    if (length(which(xx$office == "U.S. House")) == 0){
        warnmsg("Failed to find U.S. House")
    }
    if (length(which(xx$office == "State House")) == 0){
        warnmsg("Failed to find State House")
    }
    if (length(which(xx$office == "State Senate")) == 0){
        warnmsg("Failed to find State Senate")
    }
    nwhich <- length(which(xx$office == "U.S. House" & xx$district == ""))
    if (nwhich > 0){
        errmsg(paste0("Found ",nwhich," U.S. House missing district"))
    }
    nwhich <- length(which(xx$office == "State House" & xx$district == ""))
    if (nwhich > 0){
        errmsg(paste0("Found ",nwhich," State House missing district"))
    }
    nwhich <- length(which(xx$office == "State Senate" & xx$district == ""))
    if (nwhich > 0){
        errmsg(paste0("Found ",nwhich," State Senate missing district"))
    }
    # Comment out following test if no Senate race in this state for this election
    if (length(which(xx$office == "U.S. Senate")) == 0){
        errmsg("Failed to find U.S. Senate")
    }
    which_govx <- which(grepl("Governor ",xx$office,ignore.case = TRUE))
    if (length(which_govx > 0)){
        errmsg(paste0("Found offices containing 'Governor ': ",unique(xx$office[which_govx])))
    }
    return(xx)
}
cc <- fl_county_codes
# cc <- c("DAD") #DEBUG-TEST Miami-Dade only
# fl_counties <- c("Miami-Dade") #DEBUG-TEST Miami-Dade only
for (i in 1:length(cc)){
    zcounty <<- cc[i]
    #dd <- read_delim(paste0(zindir,"2018-general-election-rev/",
    dd <- read_delim(paste0(zindir,"precinctlevelelectionresults2018gen/", #DEBUG2018
                            cc[i],"_PctResults20181106.txt"), delim = '\t', quote = "",
                     col_names = FALSE, col_types = "ccdccccddddccdccddd")
    #Define names if col_names == FALSE
    names(dd) <- c("Code","COUNTY","ElectNo","ElectDate","ElectName",
                   "AreaId","AREA","RegAll","RegRep","RegDem",
                   "RegOth","Contest","DIST","ConCode","Candidate",
                   "Party","RegId","CandNo","Votes")
    if (cc[i] %in% c("ALA","BAK","BRO","CLM","DES",
                     "GLA","HER","LEO","MAD","MON",
                     "PUT","STL")){
        dd$AREA <- paste0(dd$AreaId,":",dd$AREA)
    }
    else if (cc[i] == "OSC"){
        dd$AREA <- paste0(dd$AreaId,dd$AREA)
    }
    zdd <<- dd #DEBUG-RM
    yy <- dd %>% group_by(COUNTY,AREA) %>% summarize(min=min(RegAll),max=max(RegAll),mean=mean(RegAll),
                                                     maxr=max(RegRep),maxd=max(RegDem),maxo=max(RegOth))
    nrange <- NROW(yy[yy$min < yy$max,])
    nmaxd  <- NROW(yy[yy$maxd > 0,])
    nmaxr  <- NROW(yy[yy$maxr > 0,])
    nmaxo  <- NROW(yy[yy$maxo > 0,])
    if (nrange > 0){
        errmsg(paste0(nrange," AREAs have different # of Registered Voters"))
    }
    if (nmaxd > 0){
        errmsg(paste0(nmaxd," AREAs have nonzero Democratic Registered Voters"))
    }
    if (nmaxr > 0){
        errmsg(paste0(nmaxr," AREAs have nonzero Republican Registered Voters"))
    }
    if (nmaxo > 0){
        errmsg(paste0(nmaxo," AREAs have nonzero Other Registered Voters"))
    }
    yy$Contest   <- "Registered Voters"
    yy$DIST      <- ""
    yy$Party     <- ""
    yy$Candidate <- ""
    yy$Votes     <- yy$mean
    yy <- yy[,!(names(yy) %in% c("min","max","mean","maxd","maxr","maxo"))]
    zyy <<- yy #DEBUG-RM
    xx <- data.frame(dd$COUNTY,dd$AREA,dd$Contest,dd$DIST,dd$Party,dd$Candidate,dd$Votes,
                     stringsAsFactors = FALSE)
    yy <- as.data.frame(yy)
    names(xx) <- c("COUNTY","AREA","Contest","DIST","Party","Candidate","Votes")
    xx <- rbind(xx,yy)
    county <- fl_counties[i]
    state <- "FL"
    xx <- parsefile(county, state, xx)
    zxx <<- xx #DEBUG-RM
    infomsg(paste0(names(xx),collapse = "|"))
}

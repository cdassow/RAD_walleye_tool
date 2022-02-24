#
# Walleye decision-support tool example
#
# Contact Colin Dassow via email at colin.dassow@wisconsin.gov for questions or more information


rm(list=ls())
# not all of these packages are necessary for every application of this tool to different species or regions. However, there are some core packages (shiny, DT, tidyverse) that are likely to be used in an adaptation of this tool.
library(shiny)
library(DT)
library(ggpubr)
library(tidyverse)
library(sf)
library(gmapsdistance)
library(maps)
library(ggmap)
library(keyring)

##### Preamble #####
# setting working directory and loading/cleaning data and functions to be used in the app
#setwd("~/replacementCosts/rcApp")
curdb=read.csv("curClassRec.csv") # data frame containing current data for each of the lakes in the app
curdb$Final.Lake.Class[is.na(curdb$Final.Lake.Class)]="Unclassified" # little bit of cleaning up of the data frame before it goes into the app
## More information on curb data frame, unused columns removed here to make it easier to read. In the code for the tool itself they are not

# 'data.frame':	6392 obs. of  27 variables:
# $ WBIC                                             : num  176000 1374300 1377700 1374800 176600 ...
# $ Lake.Name                                        : chr  "Amey Pond" "Arkdale Lake (Millpond)" "Arrowhead Lake (Manchester)" "Big Roche A Cri Lake" ...
# $ County                                           : chr  "Adams" "Adams" "Adams" "Adams" ...
# $ Area..acres.                                     : int  56 55 350 205 7 445 13955 48 35 24 ...
# $ Area..ha.                                        : num  22.7 22.3 141.6 83 2.8 ...
# $ Max.Depth..ft.                                   : int  7 6 30 20 5 24 36 56 47 10 ...
# $ Max.Depth..m.                                    : num  2.1 1.8 9.1 6.1 1.5 7.3 11 17.1 14.3 3 ...
# $ Lake.Class.2020                                  : chr  "Simple - Warm - Dark" "Simple - Riverine" "Complex - Warm - Dark" "Complex - Riverine" ...
# $ Temp.Transitional                                : chr  "n/a" "n/a" "no" "n/a" ...
# $ Clarity.Transitional                             : chr  "n/a" "n/a" "no" "n/a" ...
# $ If.not.then..for.2.story.and.stocked.trout.lakes.: chr  "n/a" "n/a" "n/a" "n/a" ...
# $ Hectares                                         : num  29.3 16.4 119.4 NA NA ...
# $ MaxDepth                                         : num  2.1 1.8 9 NA NA NA 10.8 16.8 14.1 3 ...
# $ ConductanceF                                     : num  226 218 319 NA NA ...
# $ landscapeposition                                : int  1 4 3 NA NA NA 6 -3 -3 3 ...
# $ Upstream.Prob.Adults                             : num  0.072 0.785 0.751 NA NA NA 0.982 0 0 0.124 ...
# $ growing.degree.days.5c                           : num  2863 2851 2890 NA NA ...
# $ probability.adult.presence                       : num  0.109 0.09 0.881 NA NA NA 0.939 0.439 0.094 0.06 ...
# $ adult.presence.binary.1                          : int  0 0 1 NA NA NA 1 0 0 0 ...
# $ probability.recruitment.1                        : num  0.077 0.31 0.325 NA NA NA 0.935 0.081 0.081 0.228 ...
# $ Recruitment.binary.1                             : int  0 0 0 NA NA NA 1 0 0 0 ...
# $ prob.adult.Presence_LateCentury                  : num  0.338 0.311 0.773 NA NA ...
# $ prob.adult.Presence_MidCentury                   : num  0.403 0.347 0.798 NA NA ...
# $ prob.Recruit_LateCentury                         : num  0.087 0.317 0.335 NA NA NA 0.903 0.092 0.092 0.236 ...
# $ prob.Recruit_MidCentury                          : num  0.087 0.317 0.335 NA NA NA 0.903 0.092 0.092 0.236 ...
# $ MidCentury.WalleyeClass                          : chr  "Non-walleye lakes" "Non-walleye lakes" "Remains stocking opportunity" NA ...
# $ LateCentury.WalleyeClass                         : chr  "Non-walleye lakes" "Non-walleye lakes" "Remains stocking opportunity" NA ...


futdb=read.csv("futClassRec.csv") # data frame containing future projections of lake class for each of the lakes in the app
# a few lines cleaning up this data frame as well to standardize it with the curdb data frame
futdb$Final.Lake.Class[is.na(futdb$Final.Lake.Class)]="Unclassified"
futdb$Final.Lake.Class=gsub("Complex ","Complex - ",futdb$Final.Lake.Class)
futdb$Final.Lake.Class=gsub("Simple ","Simple - ",futdb$Final.Lake.Class)
futdb$Final.Lake.Class=gsub("Cool ","Cool - ",futdb$Final.Lake.Class)
futdb$Final.Lake.Class=gsub("Warm ","Warm - ",futdb$Final.Lake.Class)
futdb$Final.Lake.Class=gsub("Harsh ","Harsh - ",futdb$Final.Lake.Class)

# More information on future db, unused columns removed here to make it easier to read. In the code for the tool itself they are not removed
# 'data.frame':	6392 obs. of  16 variables:
# $ WBIC                                             : num  176000 1374300 1377700 1374800 176600 ...
# $ Lake.Name                                        : chr  "Amey Pond" "Arkdale Lake (Millpond)" "Arrowhead Lake (Manchester)" "Big Roche A Cri Lake" ...
# $ County                                           : chr  "Adams" "Adams" "Adams" "Adams" ...
# $ Area..acres.                                     : int  56 55 350 205 7 445 13955 48 35 24 ...
# $ Area..ha.                                        : num  22.66 22.26 141.64 82.96 2.83 ...
# $ Max.Depth..ft.                                   : int  7 6 30 20 5 24 36 56 47 10 ...
# $ Max.Depth..m.                                    : num  2.13 1.83 9.14 6.1 1.52 ...
# $ Temp.Transitional                                : chr  NA NA "no" NA ...
# $ Clarity.Transitional                             : chr  NA NA "no" NA ...
# $ If.not.then..for.2.story.and.stocked.trout.lakes.: chr  NA NA NA NA ...
# $ Lake.Class.2050                                  : chr  "Unclassified" "Unclassified" "Complex - Warm - Dark" "Complex - Riverine" ...
# $ probability.recruit.1                            : num  0.087 0.317 0.335 NA NA NA 0.903 0.092 0.092 0.236 ...
# $ pP_Mid                                           : num  0.403 0.347 0.798 NA NA ...

regs=readRDS("2020_lake_regulations.Rds") # fishing regulations for individual waterbodies in the state of Wisconsin
regs$species[regs$species=="walleye"]="walleye_sauger_and_hybrids" #combining plain walleye with the hybrid group since they are managed together by WDNR
regs$species[regs$species=="sauger_and_hybrids"]="walleye_sauger_and_hybrids" #combining plain sauger and hybrids with walleye - from Alex Latzka

# more information on fishing regulation data, unused columns removed here to make it easier to read. In the code for the tool itself they are not removed
# 'data.frame':	302622 obs. of  19 variables:
# $ wbic                 : int  176000 176000 176000 176000 176000 176000 176000 176000 176000 176000 ...
# $ waterbody.name       : chr  "amey_pond" "amey_pond" "amey_pond" "amey_pond" ...
# $ year                 : int  2020 2020 2020 2020 2020 2020 2020 2020 2020 2020 ...
# $ county               : chr  "adams" "adams" "adams" "adams" ...
# $ species              : chr  "all_species" "catfish" "cisco_and_whitefish" "largemouth_bass_and_smallmouth_bass" ...
# $ reg.source           : chr  "state" "state" "state" "wbic" ...
# $ reg.type             : int  8 4 4 5 4 4 3 3 4 3 ...
# $ species.group        : chr  "all_species" "all_species" "all_species" "all_species" ...
# $ min.length           : num  0 NA NA NA NA NA 15 40 NA 32 ...
# $ max.length           : num  NA NA NA 18 NA NA NA NA NA NA ...
# $ bag.limit            : num  NA 10 10 5 25 999 5 1 999 1 ...
# $ gear.restriction     : chr  "Motor Trolling is allowed with up to 3 hooks, baits, or lures, per angler." NA NA NA ...
# $ other.restrictions   : chr  "Catch and release fishing for largemouth and smallmouth bass is open year round unless otherwise noted." NA NA NA ...
# $ start.date           : Date, format: "2020-04-01" "2020-04-01" "2020-04-01" "2020-05-02" ...
# $ end.date             : Date, format: "2021-03-31" "2021-03-31" "2021-03-31" "2021-03-07" ...
# $ functional.start.date: Date, format: "2015-07-01" NA NA "2018-04-01" ...
# $ functional.end.date  : Date, format: NA NA NA NA ...
# $ reg.type.desc        : chr  "Gear Restriction" "No minimum length limit and the daily bag limit is 10." "No minimum length limit and the daily bag limit is 10." "No minimum length, but largemouth bass and smallmouth bass from 14\" to 18\" may not be kept, and only 1 fish o"| __truncated__ ...
# $ comments             : chr  NA NA NA NA ...

stoSum=readRDS("Walleye_stocking_summary_1972_to_present.Rds") # stocking summaries for each walleye stocking event in the state of Wisconsin from 1972 to 2020

# # more information on stocking summary data, unused columns removed here to make it easier to read. In the code for the tool itself they are not removed
# 'data.frame':	13051 obs. of  10 variables:
# $ county              : chr  "iowa" "lincoln" "iron" "iron" ...
# $ waterbody.name      : chr  "sudan_br" "pine_lake" "north_bass_lake" "north_bass_lake" ...
# $ wbic                : int  929000 1012100 1868900 1868900 1569900 2390800 2390800 2390800 2390800 2390800 ...
# $ year                : int  2010 2020 2008 2006 2005 2008 2018 2017 2018 2020 ...
# $ species             : chr  "walleye" "walleye" "walleye" "walleye" ...
# $ age.class           : chr  "small_fingerling" "large_fingerling" "small_fingerling" "small_fingerling" ...
# $ number.fish.stocked : int  330 661 6300 6338 3820 16510 7389 1000 21000 2400 ...
# $ mean.length         : num  1.66 6.7 1.4 1.6 7.1 7.2 7.3 6.7 1.6 7.6 ...
# $ length.unit         : chr  "in" "in" "in" "in" ...
# $ mark.given          : logi  NA NA NA NA NA NA ...

crCPE=read.csv("creel_raw_interview_fish_data_ALL.csv") # creel catch per unit effort survey data
crCPE$Caught.Amt=gsub("-","0",crCPE$Caught.Amt) #replacing (-) with 0 for caught amount, there is no difference in meaning between (-) and 0

# # more information on creel catch per unit effort data, unused columns removed here to make it easier to read. In the code for the tool itself they are not removed

# 'data.frame':	407886 obs. of  18 variables:
# $ County              : chr  "FOREST" "FOREST" "FOREST" "FOREST" ...
# $ Waterbody.Name      : chr  "ROBERTS LAKE" "ROBERTS LAKE" "ROBERTS LAKE" "ROBERTS LAKE" ...
# $ WBIC                : int  378400 378400 378400 378400 378400 378400 378400 378400 378400 378400 ...
# $ Survey.Year         : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...
# $ Srvy.Seq.No         : int  95943 95943 95943 95943 95943 95943 95943 95943 95943 95943 ...
# $ Visit.Fish.Seq.No   : int  517061 517061 517061 517061 517064 517064 517064 517065 517065 517065 ...
# $ Int.Party.Seq.No    : int  4264886 4264886 4264887 4264887 4264888 4264888 4264888 4264889 4264890 4264890 ...
# $ Anglers.Amt         : num  2 2 2 2 2 2 2 2 4 4 ...
# $ Nonanglers.Amt      : chr  "0" "0" "0" "0" ...
# $ Start.Time          : int  1330 1330 1200 1200 1300 1300 1300 1300 1100 1100 ...
# $ End.Time            : int  1537 1537 1604 1604 1700 1700 1700 1627 1607 1607 ...
# $ Not.Fishing.Amt     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Fish.Species.Code   : chr  "L02" "X22" "L02" "X22" ...
# $ Fished.Perc         : chr  "100" "100" "100" "100" ...
# $ Caught.Amt          : chr  "0" "0" "3" "0" ...
# $ Kept.Amt            : chr  "0" "0" "0" "0" ...
# $ Fish.Comment        : chr  "-" "-" "-" "-" ...
# $ Release.Amt         : chr  "-" "-" "-" "-" ...


crLen=read.csv("creel_raw_fish_length_data_interview_ALL.csv") # lengths of fish caught by anglers in the creel survey. These are measured by the creel clerk during the interview
crLen$First.Mark.Found=gsub("-",NA,crLen$First.Mark.Found) #replacing "-" with NA to make it easier to work with the data 

# # more information on creel length measurement data, unused columns removed here to make it easier to read. In the code for the tool itself they are not removed
# 'data.frame':	214152 obs. of  16 variables:
# $ County              : chr  "BARRON" "BARRON" "BARRON" "BARRON" ...
# $ Waterbody.Name      : chr  "BEAR LAKE" "BEAR LAKE" "BEAR LAKE" "BEAR LAKE" ...
# $ WBIC                : int  2105100 2105100 2105100 2105100 2105100 2105100 2105100 2105100 2268300 2268500 ...
# $ Survey.Year         : int  2000 2000 2000 2000 2000 2000 2000 2000 1998 1998 ...
# $ Srvy.Seq.No         : int  96051 96051 96051 96051 96051 96051 96051 96051 96070 95917 ...
# $ Visit.Fish.Seq.No   : int  536793 537147 537147 537147 537147 537147 537147 537147 538636 539550 ...
# $ Sample.Date         : chr  "2/6/2001" "12/31/2000" "12/31/2000" "12/31/2000" ...
# $ Anglers.Amt         : int  1 1 1 1 1 1 1 1 2 2 ...
# $ Nonanglers.Amt      : chr  "0" "0" "0" "0" ...
# $ Start.Time          : int  915 715 715 715 915 915 915 915 600 1200 ...
# $ End.Time            : int  1115 1000 1000 1000 1130 1130 1130 1130 950 1630 ...
# $ Not.Fishing.Amt     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Fish.Count          : chr  "1" "1" "1" "1" ...
# $ Species.Code        : chr  "W14" "X15" "X15" "X15" ...
# $ Fish.Length         : chr  "10" "7.4" "10.4" "9.5" ...
# $ Fish.Length.Units   : chr  "IN" "IN" "IN" "IN" ...

cpe=read.csv("electrofishingCPEs.csv",stringsAsFactors = F) # standardized electrofishing catch per unit effort data
cpe$cpeMeasure=factor(cpe$cpeMeasure,levels = c("CPEmile","CPE3","CPE5","CPE6","CPE7","CPE8","CPE10","CPE11","CPE12","CPE14","CPE15","CPE20")) #getting CPE measures in a useful order

# # more information on standardized catch per unit effort data
# 'data.frame':	52548 obs. of  5 variables:
# $ WBIC      : int  93100 93100 93100 93100 93100 93100 93100 93100 93100 93100 ...
# $ surveyYear: int  2008 2008 2008 2008 2008 2008 2008 2008 2008 2008 ...
# $ species   : chr  "SMALLMOUTH BASS" "SMALLMOUTH BASS" "SMALLMOUTH BASS" "SMALLMOUTH BASS" ...
# $ cpeMeasure: Factor w/ 12 levels "CPEmile","CPE3",..: 1 5 6 8 9 10 12 2 3 4 ...
# $ cpeVal    : num  5.9 5.88 5.88 5.59 4.71 2.35 0.29 NA NA NA ...

pe2021=read.csv("WAEpopest2021.csv",stringsAsFactors = F) # mark-recapture population estimates for adult walleye
pes=pe2021[,c(1:5,8:10,15:22,30,42:46,49:50)] #trimming off unimportant columns because of the size of the file

# # more information on adult walleye population estimate data
# 'data.frame':	2196 obs. of  24 variables:
# $ WBIC       : chr  "9999999" "70" "70" "70" ...
# $ COUNTY     : int  9999999 5 5 5 15 15 15 15 15 15 ...
# $ WATER      : int  9999 10 10 10 999 999 999 999 999 999 ...
# $ REPNAME    : chr  "A very long lake name" "FOX R. MOU" "FOX R. MOU" "FOX R. MOU" ...
# $ System     : chr  "AAAAAAAAAAA" "" "" "" ...
# $ NUMACRE    : num  99999 NA NA NA NA ...
# $ LBSACRE    : int  999999 NA NA NA NA NA NA NA NA NA ...
# $ SPECIES    : chr  "x99" "X22" "X22" "X22" ...
# $ NUMBER     : num  999999 60000 18732 192556 1840 ...
# $ LBS        : int  9999 NA NA NA NA NA NA NA NA NA ...
# $ NUMHIGH    : num  999999 NA NA 111691 3600 ...
# $ NUMLOW     : int  999999 NA NA 273420 860 4500 11200 7692 31707 16250 ...
# $ LBSHIGH    : int  999999 NA NA NA NA NA NA NA NA NA ...
# $ LBSLOW     : int  999999 NA NA NA NA NA NA NA NA NA ...
# $ REPAREA    : num  99999 1752 1752 1752 NA ...
# $ ModelINCode: int  999 10 10 10 10 10 10 10 10 10 ...
# $ MYEAR1     : int  9999 1981 1982 1983 1976 1977 1978 1979 1980 1981 ...
# $ WRC        : chr  "XXXXX" "" "" "" ...
# $ Males      : chr  "9999999" "" "" "" ...
# $ CVM        : chr  "9.999999" "" "" "" ...
# $ Females    : chr  "9999999" "" "" "" ...
# $ CVF        : chr  "9.999999" "" "" "" ...
# $ TPE        : int  9999999 NA NA NA NA NA NA NA NA NA ...
# $ CVTPE      : num  10 NA NA NA NA ...

yoyCPE=read.csv("wlyYOY_survey.csv",stringsAsFactors = F) # fall young of year recruitment surveys (expressed as number caught per mile of shoreline sampled)

# # more information on young of year walleye data
# 'data.frame':	8241 obs. of  28 variables:
# $ WBIC    : int  2339900 2339900 2339900 2339900 2339900 2339900 2339900 2339900 2339900 2339900 ...
# $ STATE   : chr  "WI" "WI" "WI" "WI" ...
# $ COUNTY  : chr  "VILAS" "VILAS" "VILAS" "VILAS" ...
# $ LAKE    : chr  "ESCANABA L" "ESCANABA L" "ESCANABA L" "ESCANABA L" ...
# $ AREA    : chr  "293" "293" "293" "293" ...
# $ YEAR    : int  1958 1958 1958 1958 1958 1958 1958 1958 1958 1958 ...
# $ AGE0CPE : chr  "10.1532567" "11.302682" "2.2988506" "14.559387" ...
# $ AGE1CPE : chr  NA NA NA NA ...
# $ CODE    : chr  "NR" "NR" "NR" "NR" ...
# $ CRNTCODE: chr  "NR" "NR" "NR" "NR" ...
# $ MILESURV: num  5.22 5.22 5.22 5.22 5.22 5.22 5.22 5.22 5.22 5.22 ...
# $ SHORMILE: num  5.22 5.22 5.22 5.22 5.22 5.22 5.22 5.22 5.22 5.22 ...
# $ PERCSURV: num  100 100 100 100 100 100 100 100 100 100 ...
# $ AGE0WAE : chr  "53" "59" "12" "76" ...
# $ AGE0MINL: chr  NA NA NA NA ...
# $ AGE0MAXL: chr  NA NA NA NA ...
# $ AGE0AVEL: chr  NA NA NA NA ...
# $ AGE1WAE : chr  NA NA NA NA ...
# $ AGE1MINL: chr  NA NA NA NA ...
# $ AGE1MAXL: chr  NA NA NA NA ...
# $ AGE1AVEL: chr  NA NA NA NA ...
# $ TOTALWAE: chr  NA NA NA NA ...
# $ COMMENT : chr  "" "" "" "" ...
# $ TEMP    : num  64 67 67 64 63 66 65 60 60 62 ...
# $ CLARITY : chr  NA NA NA NA ...
# $ RELIABLE: chr  "" "" "" "" ...
# $ HOURS   : num  NA NA NA NA NA NA NA NA NA NA ...

inv=read.csv("invasiveSpp.csv") #invasive species presence/absence (coded as 1/0) data as of 2020

#more information on the invasive species data

# 'data.frame':	2873 obs. of  66 variables:
# $ WaterBodyName                                     : chr  "Amey Pond" "Arkdale Lake" "Big Roche A Cri Creek" "Big Roche a Cri" ...
# $ WBIC                                              : num  176000 1374300 1374100 1374800 1378100 ...
# $ invList                                           : chr  "Banded Mystery Snail, Curly-Leaf Pondweed, Eurasian Water-Milfoil" "Chinese Mystery Snail, Curly-Leaf Pondweed, Eurasian Water-Milfoil, Purple Loosestrife, Rusty Crayfish, Water H"| __truncated__ "Japanese knotweed (Fallopia japonica), Rusty Crayfish, Water Hyacinth, Zebra Mussel" "Chinese Mystery Snail, Curly-Leaf Pondweed, Eurasian Water-Milfoil, Japanese knotweed (Fallopia japonica), Rust"| __truncated__ ...
# $ Banded.Mystery.Snail                              : int  1 0 0 0 1 0 1 1 0 1 ...
# $ Curly.Leaf.Pondweed                               : int  1 1 0 1 1 0 1 1 1 1 ...
# $ Eurasian.Water.Milfoil                            : int  1 1 0 1 1 1 1 1 0 1 ...
# $ Chinese.Mystery.Snail                             : int  0 1 0 1 1 0 0 0 0 0 ...
# $ Purple.Loosestrife                                : int  0 1 0 0 1 0 0 0 0 0 ...
# $ Rusty.Crayfish                                    : int  0 1 1 1 0 0 0 0 0 0 ...
# $ Water.Hyacinth                                    : int  0 1 1 0 0 0 0 0 0 0 ...
# $ Zebra.Mussel                                      : int  0 1 1 1 1 1 0 0 0 0 ...
# $ Japanese.knotweed..Fallopia.japonica.             : int  0 0 1 1 0 0 0 0 0 0 ...
# $ Hybrid.Eurasian...Northern.Water.Milfoil          : int  0 0 0 0 1 0 1 1 0 1 ...
# $ Cylindro                                          : int  0 0 0 0 0 1 0 0 0 0 ...
# $ Brittle.Waternymph                                : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Phragmites..non.native.                           : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Yellow.Iris                                       : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Freshwater.Jellyfish                              : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Ornamental.water.lilies..non.native.Nymphaea.sp...: int  0 0 0 0 0 0 0 0 0 0 ...
# $ Rainbow.Smelt                                     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Queen.of.the.meadow..Filipendula.ulmaria.         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Hybrid.Cattail                                    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Narrow.leaf.cattail..Typha.angustifolia.          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Japanese.Mystery.Snail                            : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Aquatic.forget.me.not..Myosotis.scorpioides.      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Round.Goby                                        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Asiatic.Clam..Corbicula.                          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ White.Perch                                       : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Viral.Hemorrhagic.Septicemia                      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Ruffe                                             : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Bighead.Carp                                      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Daphnia.lumholtzii.                               : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Silver.Carp                                       : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Common.Carp                                       : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Faucet.Snail                                      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Flowering.Rush                                    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ New.Zealand.Mudsnail                              : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Japanese.Hops                                     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Quagga.Mussel                                     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Southern.Cattail                                  : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Spiny.Waterflea                                   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Water.Lettuce                                     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Goldfish                                          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Koi                                               : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Grass.Carp                                        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Reed.Manna.Grass                                  : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Starry.Stonewort                                  : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Garden.heliotrope..Valeriana.officinalis.         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Spiny.Naiad                                       : int  0 0 0 0 0 0 0 0 0 0 ...
# $ European.marsh.thistle                            : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Yellow.Floating.Heart                             : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Golden.Creeper                                    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Western.Mosquitofish                              : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Moneywort..Lysimachia.nummularia.                 : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Threespine.Stickleback                            : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Hairy.Willow.Herb                                 : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Alewife                                           : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Graceful.Cattail                                  : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Lyme.Grass                                        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Lesser.celandine..Ranunculus.ficaria.             : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Java.Waterdrop...Vietnamese.Water.Celery          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Butterfly.dock                                    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Pennywort                                         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Sacred.Lotus                                      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Red.Swamp.Crayfish                                : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Seaside.Goldenrod                                 : int  0 0 0 0 0 0 0 0 0 0 ...

travelDist=read.csv("travel_dist_CityZip2_WBIC.csv") # travel distances from key Wisconsin cities to each of the lakes in the decision-support tool

# more information on the structure of the travel distance data frame
# 'data.frame':	3426944 obs. of  3 variables:
# $ CityZip: chr  "Racine " "Racine " "Racine " "Racine " ...
# $ WBIC   : int  5504355 2964000 5504251 2964700 5504240 2963800 2964100 2964300 5504414 5504355 ...
# $ km     : num  453 453 453 453 453 ...

costs=read.csv("../Increased_costs_2017USD_mid.csv") # calculations of increased travel costs (according to AAA cost guide and median incomes from 2010 census)

# more information on the increased travel costs data frame
# 'data.frame':	656 obs. of  7 variables:
# $ CityZip   : chr  "Abbotsford" "Abrams" "Adams" "Adell" ...
# $ MinCurGood: num  43.9 87.7 98.7 218.3 246.4 ...
# $ MinMidGood: num  43.9 87.7 98.7 218.3 246.4 ...
# $ ShiftDist : num  0 0 0 0 0 0 0 0 0 0 ...
# $ MED_INC   : num  44052 62017 32346 69063 55481 ...
# $ IncCost   : num  0 0 0 0 0 0 0 0 0 0 ...


#subsetting cost data down to the cities we want to focus on for one of the tables displayed on the travel costs tab
cities=c("Milwaukee ","Madison ","Green Bay ","Wausau ","Eau Claire " ,"La Crosse ","Hayward ","Superior ") 
td=travelDist[travelDist$CityZip%in%cities,];td=td[!is.na(td$WBIC),]
#same for distance to nearest quality fishery
cities2=c("Milwaukee","Madison","Green Bay","Wausau","Eau Claire" ,"La Crosse","Hayward","Superior") 
nearGood=costs[costs$CityZip%in%cities2,c(2:7)]
nearGood$MinCurGood*0.62;nearGood$MinMidGood*0.62;nearGood$ShiftDist*0.62 # going from kilometers to miles
nearGood$IncCost*1.06 #going from 2017 dollars to 2020
nearGood$MED_INC*1.17 # going from 2011 dollars to 2020
colnames(nearGood)=c("City","Min. Dist. to Current Quality Fishery (mi)","Min. Dist. to Mid-Century Quality Fishery (mi)","Change in Distance (mi)","Median Income","Increased Cost (2020 dollars)")

#reading in a spatial data layer to be used in calculating travel distances in costs tab.
hdro=st_read("~/replacementCosts/24k_Hydro_Waterbodies_(Open_Water).shp")


# I needed to create a translation list that takes different versions of species names from all the different data sources and links them to a master list of species.
# that file is now read in as 'masterSpeciesKey.csv'
spk=read.csv("masterSpeciesKey.csv")
# some more cleaning and organizing of that file before it gets used
sppChoices=c("Northern pike","Muskellunge","Yellow perch","Green sunfish","Pumpkinseed","Warmouth","Bluegill","Smallmouth bass","Largemouth bass","White crappie","Black crappie","Walleye") 
spk=spk[spk$Common.Name%in%sppChoices,]# the only species to be considered in the app

#grouping these together as 'walleye' since that is how they are managed by WDNR
spk$regsNames[spk$regsNames=="walleye"]="walleye_sauger_and_hybrids" 
spk$regsNames[spk$regsNames=="sauger_and_hybrids"]="walleye_sauger_and_hybrids"

# more information on the species key, not critical for understanding the app, but in case this information is useful, here it is.
# 'data.frame':	30 obs. of  4 variables:
# $ Fish.Species.Code: chr  "L02" "L03" "W05" "W06" ...
# $ Common.Name      : chr  "Northern pike" "Muskellunge" "Green sunfish" "Pumpkinseed" ...
# $ regsNames        : chr  "northern_pike" "muskellunge_and_hybrids" "panfish" "panfish" ...
# $ standCPE         : chr  NA NA NA NA ...

# several functions from the key ring package are used here but because they contain sensitive login information only the functions used are named here and the login information is removed.
# key_set('the information here is not provided as it's sensitive account login information')
# register_google(key_get('the information here is not provided as it's sensitive account login information))
# set.api.key(key_get('the information here is not provided as it's sensitive account login information))

#custom functions to be used in the app, these are for calculating driving times and distances in the travel costs/alternative fishing options tab of the app

closestCur = function(n,city="Stevens Point",stateAbrv="WI",curClass){
  lClasses=unique(curClass$Final.Lake.Class)
  city=paste(city,stateAbrv,sep = ", ")
  # key_set('the information here is not provided as it's sensitive account login information')
  # register_google(key_get('the information here is not provided as it's sensitive account login information))
  # set.api.key(key_get('the information here is not provided as it's sensitive account login information))
  
  ll = geocode(city)
  ll$city=city
  city_sf = st_as_sf(ll,coords=c("lon","lat"),crs=4326)
  datToUse=curClass[curClass$probability.recruit.1>0.499,]
  center=st_as_sf(datToUse[!is.na(datToUse$Latitude),],coords = c("Longitude","Latitude"),crs=4326) # only consider lakes with 0.5 NR prob or better <- see if there should be a better filter on this
  dists = st_distance(city_sf,center)
  colnames(dists)=center$WBIC
  out=list()
  for(i in 1:(length(lClasses)-1)){# to avoid looping through "unclassified lakes" since they don't have latLongs, this will be different for future projections
    nClose1=sort(dists[1,which(colnames(dists)%in%center$WBIC[center$Final.Lake.Class==lClasses[i]])]) #gives me the WBICs and distance (in m) of n closest lakes to the chosen city 
    if(length(nClose1)<n){nClose=nClose1}else{nClose=nClose1[1:n]}
    if(!is.null(names(nClose))){addWBIC=as.numeric(names(nClose))}else{addWBIC=NA}
    if(!is.null(names(nClose))){addDist=nClose}else{addDist=NA}
    addDat=data.frame(WBIC=addWBIC,distance.m=addDist)
    addDat=dplyr::left_join(addDat,center,by="WBIC")
    out[[i]]=addDat
  }
  names(out)=lClasses[1:15]
  return(out)
}

closestFut = function(n,city="Stevens Point",stateAbrv="WI",futClass){
  lClasses=unique(futClass$Final.Lake.Class[!is.na(futClass$Latitude)])
  city=paste(city,stateAbrv,sep = ", ")
  # key_set('the information here is not provided as it's sensitive account login information')
  # register_google(key_get('the information here is not provided as it's sensitive account login information))
  # set.api.key(key_get('the information here is not provided as it's sensitive account login information))
  
  ll = geocode(city)
  ll$city=city
  city_sf = st_as_sf(ll,coords=c("lon","lat"),crs=4326)
  datToUse=futClass[futClass$probability.recruit.1>0.499,]
  center=st_as_sf(datToUse[!is.na(datToUse$Latitude),],coords = c("Longitude","Latitude"),crs=4326) # only consider lakes with 0.5 NR prob or better <- see if there should be a better filter on this
  dists = st_distance(city_sf,center)
  colnames(dists)=center$WBIC
  out=list()
  for(i in 1:(length(lClasses))){
    nClose1=sort(dists[1,which(colnames(dists)%in%center$WBIC[center$Final.Lake.Class==lClasses[i]])]) #gives me the WBICs and distance (in m) of n closest lakes to the chosen city 
    if(length(nClose1)<n){nClose=nClose1}else{nClose=nClose1[1:n]}
    if(!is.null(names(nClose))){addWBIC=as.numeric(names(nClose))}else{addWBIC=NA}
    if(!is.null(names(nClose))){addDist=nClose}else{addDist=NA}
    addDat=data.frame(WBIC=addWBIC,distance.m=addDist)
    addDat=dplyr::left_join(addDat,center,by="WBIC")
    out[[i]]=addDat
  }
  names(out)=lClasses
  return(out)
}
driveTime=function(city="Stevens Point",stateAbrv="WI",closestLakeOutput){
  dat=closestLakeOutput
  city=paste(city,stateAbrv,sep = ", ")
  # key_set('the information here is not provided as it's sensitive account login information')
  # register_google(key_get('the information here is not provided as it's sensitive account login information))
  # set.api.key(key_get('the information here is not provided as it's sensitive account login information))
  
  ll = geocode(city)
  out=data.frame(startName=character(),
                 startLat=numeric(),
                 startLong=numeric(),
                 endWBIC=numeric(),
                 endName=character(),
                 endLat=numeric(),
                 endLong=numeric(),
                 distanceStraight_m=numeric(),
                 distanceDrive_m=numeric(),
                 timeDriving_s=numeric(),
                 lakeClass=character())
  for(i in 1:length(dat)){
    startName=rep(city,nrow(dat[[i]]))
    startLat=rep(ll$lat,nrow(dat[[i]]))
    startLong=rep(ll$lon,nrow(dat[[i]]))
    endWBIC=dat[[i]]$WBIC
    endName=dat[[i]]$Lake.Name
    if(length(dat[[i]]$geometry)>1){
      endLat=as.vector(as.data.frame(st_coordinates(dat[[i]]$geometry))[2])
      endLong=as.vector(as.data.frame(st_coordinates(dat[[i]]$geometry))[1])
    }else{endLat=NA;endLong=NA}
    distanceStraight_m=dat[[i]]$distance.m
    lakeClass=rep(names(dat)[i],nrow(dat[[i]]))
    #making table of coords for gmapdistance
    if(length(dat[[i]]$geometry)==1){coords=data.frame(Longitude=NA,Latitude=NA)
    addDat=data.frame(startName,startLat,startLong,endWBIC,endName,NA,NA,NA,NA,NA,lakeClass)
    colnames(addDat)=colnames(out)
    out=rbind(out,addDat)
    }else{coords=as.data.frame(st_coordinates(dat[[i]]$geometry));colnames(coords)=c("Longitude","Latitude")
    coords$lalo=paste(coords$Latitude,coords$Longitude,sep = "+")
    dds=gmapsdistance(origin = paste(ll$lat,ll$lon,sep = "+"),destination = coords$lalo,mode = "driving",shape="long")
    distanceDrive_m=dds$Distance$Distance
    timeDriving_s=dds$Time$Time
    addDat=data.frame(startName,startLat,startLong,endWBIC,endName,endLat,endLong,distanceStraight_m,distanceDrive_m,timeDriving_s,lakeClass)
    colnames(addDat)=colnames(out)
    out=rbind(out,addDat)
    }
  }
  return(out)
}


##### UI #####

# Define User Interface for application
# for detailed information on the structure and organization of shiny app see: https://shiny.rstudio.com/tutorial/

ui <- fluidPage(
  # begin by setting up WDNR standard aesthetics for how the UI should appear
  toupper("Wisconsin Department of Natural Resources Walleye Decision Making Tool"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "minimal-v1.css"),
    includeHTML("www/minbrandheader.inc"),

    
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Fira+Sans&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Anton&display=swap');
      * {
        font-family: 'Anton', sans-serif;
      }
      h4 {
        font-family: 'Anton', sans-serif;
      }
      main-header {
        font-family: 'Anton', sans-serif;
      }"))),
  
  
  # end of setting up WDNR UI aesthetics
  
  # beginning of user interface set up
  sidebarLayout(#fluid = F,
    sidebarPanel(width = 2,
                 # waterbody selection page
                 wellPanel(selectizeInput(inputId = "county",
                                          label = "Select County",
                                          choices = unique(curdb$County),
                                          options = list(maxOptions=15,
                                                         placeholder='search by County',
                                                         closeAfterSelect=T)),
                           uiOutput("lname"),
                           actionButton("submitButton","Submit",class="btn-sucess")
                 ),
                 selectizeInput(inputId = "spp",
                                label = "Choose Species Information to Display",
                                choices = unique(spk$Common.Name)[order(unique(spk$Common.Name))],
                                selected = c("Walleye"),
                                multiple=T),
                 
    ),
    mainPanel(width = 10,
              #non-tabbed main info bar, this is always visible at the top of the page
              h1("Lake Class and Walleye NR Information"),
              hr(style="border-top: 1px solid #000000;"),
              fluidRow(column(2,wellPanel(tableOutput("lakeConstants"))),
                       column(6,wellPanel(tableOutput("lakeTimeVariable"))),
                       column(4,plotOutput("lakeShape"))),
              wellPanel(fluidRow(column(9,plotOutput("lakeClassBins")),
                                 column(3,h5(textOutput("lakeClassPlotText")),))),
              #tabs below the main infor bar. These contain the differnt data types loaded in above. 
              # The tab names are located in the tabPanel() functions for example the Regulations tab name can befound in the tabPanel("Regulations",...) call
              h2("Additional Lake Information"),
              tabsetPanel(
                tabPanel("Regulations",
                         h3(textOutput("regTableName")),
                         fluidRow(
                           column(12,tableOutput("curRegs"))),
                         hr(style="border-top: 1px solid #000000;"),
                         br(),
                         br(),
                         textOutput("lakeClassRegsText"),
                         br(),
                         br(),
                         fluidRow(wellPanel(
                           checkboxGroupInput(inputId = "showCols",label = "Columns to Display",choices = names(regs)[c(1,2,4,5,7,8,10:12,14:21)],selected = names(regs)[c(1,2,4,7,20,21)],inline = T))),
                         tabsetPanel( # additional tabs within the Regulations tab panel
                           tabPanel("2020 Lake Class Regs",
                                    h3(textOutput("curClassRegs")),
                                    fluidRow(splitLayout(
                                      plotOutput("curWLY",click = "plot_clickCWLY"),
                                      DT::dataTableOutput("curWLYDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("curLMB_SMB",click = "plot_clickCLMB_SMB"),
                                      DT::dataTableOutput("curLMB_SMBDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("curLMB",click = "plot_clickCLMB"),
                                      DT::dataTableOutput("curLMBDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("curSMB",click = "plot_clickCSMB"),
                                      DT::dataTableOutput("curSMBDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("curMSK",click = "plot_clickCMSK"),
                                      DT::dataTableOutput("curMSKDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("curNPK",click = "plot_clickCNPK"),
                                      DT::dataTableOutput("curNPKDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("curPanfish",click = "plot_clickCPan"),
                                      DT::dataTableOutput("curPanfishDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("curCrappies",click = "plot_clickCCra"),
                                      DT::dataTableOutput("curCrappiesDT")))
                                    
                           ),
                           tabPanel("2050 Lake Class Regs",
                                    h3(textOutput("futClassRegs")),
                                    fluidRow(splitLayout(
                                      plotOutput("futWLY",click = "plot_clickFWLY"),
                                      DT::dataTableOutput("futWLYDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("futLMB_SMB",click = "plot_clickFLMB_SMB"),
                                      DT::dataTableOutput("futLMB_SMBDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("futLMB",click = "plot_clickFLMB"),
                                      DT::dataTableOutput("futLMBDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("futSMB",click = "plot_clickFSMB"),
                                      DT::dataTableOutput("futSMBDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("futMSK",click = "plot_clickFMSK"),
                                      DT::dataTableOutput("futMSKDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("futNPK",click = "plot_clickFNPK"),
                                      DT::dataTableOutput("futNPKDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("futPanfish",click = "plot_clickFPan"),
                                      DT::dataTableOutput("futPanfishDT"))),
                                    fluidRow(splitLayout(
                                      plotOutput("futCrappies",click = "plot_clickFCra"),
                                      DT::dataTableOutput("futCrappiesDT")))
                           )
                         )
                         
                ),
                tabPanel("Walleye Stocking", 
                         h3(textOutput("lakeStoInfoTitle")),
                         br(),
                         br(),
                         textOutput("stoDescriptiveText"),
                         br(),
                         br(),
                         wellPanel(checkboxGroupInput(inputId = "ageClass",
                                                      label = "Select Stocked Age Classes to Display",
                                                      choices = unique(stoSum$age.class)[c(1:3,7)],
                                                      selected = c("fingerling","small_fingerling","large_fingerling"),
                                                      inline = T)),
                         fluidRow(
                           column(width = 12, textOutput("stoSumReport"))
                         ),
                         h3(textOutput("stoHistTitle")),
                         fluidRow(
                           column(width = 8,offset = 2,plotOutput("stoBoxWhisk")),
                           column(width = 12, plotOutput("meanStoSumPlot"))
                         ),
                         hr(style="border-top: 1px solid #000000;"),
                         br(),
                         br(),
                         textOutput("stoCostDescrp"),
                         br(),
                         br(),
                         fluidRow(
                           column(width = 4, verticalLayout(
                             h3(textOutput("lakeStoCostsTitle")),
                             tableOutput("lakeStoCosts"),
                             h3(textOutput("lakeCumuStoCost")))),
                           column(width = 4, verticalLayout(
                             h3(textOutput("curStoCostsTitle")),
                             tableOutput("curClassCosts"),
                             h3(textOutput("curCumuStoCost"))
                           )),
                           column(width = 4, verticalLayout(
                             h3(textOutput("futStoCostsTitle")),
                             tableOutput("futClassCosts"),
                             h3(textOutput("futCumuStoCost"))
                           )),
                         )),
                tabPanel("Creel", 
                         br(),
                         br(),
                         textOutput("creDescText"),
                         br(),
                         br(),
                         wellPanel(uiOutput("creelSPP")),
                         h3(textOutput("lakeCreelInfoTitle")),
                         h4("Catch-per-unit-effort Distributions"),
                         fluidRow(column(width = 12,textOutput("crDataWarn"))),
                         fluidRow(column(width = 10,offset=1,verticalLayout(plotOutput("creelBoxWhisk"),
                                                                           h5(textOutput("creBoxWhiskText"))))),
                         br(),
                         br(),
                         h4("Catch Length Distributions"),
                         br(),
                         h5(textOutput("creLenPlotText")),
                         br(),
                         fluidRow(column(width = 12,plotOutput("lakeCreelLengths"))),
                         br(),
                         hr(style="border-top: 1px solid #000000:"),
                         br(),
                         h5(textOutput("creBagTitle")),
                         br(),
                         h5(textOutput("creBagPlotText")),
                         br(),
                         fluidRow(column(width = 12,plotOutput("creBagLimits"))),
                         br(),
                         br(),
                         br()),
                
                tabPanel("Population Metrics",
                         br(),
                         br(),
                         p("The standardized CPE Samples tab shows standardized electrofishing survey data (if available) separated by species for the selected lake, its 2020 lake class, and its 2050 lake class.The data are presented both as distributions across all available observations and as trends through time. Use the check boxes to customize which catch-per-effort measures to display. For example CP8 denotes the catch-per-mile of fish 8in or larger."),
                         br(),
                         p("The Walleye Population Estimates tab displays adult walleye population estimates from Ceded Territory Lakes only. Trends in adult population size are shown through time. Also shown are the number of adults per acre comparision between the selected lake and its current and future lake class."),
                         br(),
                         br(),
                         tabsetPanel( # additional tabs within the population metrics tab panel
                                      # tabs for Standardize CPE, Walleye pop. ests., and YOY walleye CPE
                           tabPanel("Standardized CPE Samples",
                                    h3("Young-of-year Walleye Electrofishing Catch-per-mile"),
                                    p("YOY walleye catch per mile distributions (right) and trends through time (left) for the selected lake (if they exist) and its 2020 and 2050 lake class are shown. The table presents mean values for the various YOY length metrics reported with the CPmile surveys; these metrics are shown for the selected lake and its 2020 and 2050 lake class."),
                                    fluidRow(column(width = 4,plotOutput("yoyBoxWhisk")),
                                             column(width = 8,plotOutput("yoyTemporal"))),
                                    fluidRow(column(width = 10,DT::dataTableOutput("yoyLengthTable"))),
                                    hr(style="border-top: 1px solid #000000;"),
                                    br(),
                                    h3(textOutput("cpeDistText")),
                                    wellPanel(checkboxGroupInput(inputId = "cpeMeas", 
                                                                 label = "Choose CPE Measures to Display",
                                                                 choices = levels(cpe$cpeMeasure),
                                                                 selected = "CPEmile",
                                                                 inline = T)),
                                    fluidRow(column(width = 12,plotOutput("cpeBoxWhisk"))),
                                    h3(textOutput("lakeCPEInfoTitle")),
                                    fluidRow(column(width = 12,plotOutput("lakeCPE"))),
                                    h3(textOutput("curClassCPEInfoTitle")),
                                    fluidRow(column(width = 12,plotOutput("curCPE"))),
                                    h3(textOutput("futClassCPEInfoTitle")),
                                    fluidRow(column(width = 12,plotOutput("futCPE")))),
                           tabPanel("Walleye Population Estimates",
                                    fluidRow(column(width = 5,plotOutput("lakePETime")),
                                             column(width = 7,plotOutput("classCompDens"))),
                                    textOutput("lakePEDetailText"),
                                    DT::dataTableOutput("lakeDetailData"))),
                         
                ),
                tabPanel("Alternative Walleye Lakes & Travel Costs",
                         br(),
                         tabsetPanel( # additional tabs within the alternative options and travel costs tab
                           tabPanel("Substitutable Walleye Options",
                                    br(),
                                    p("Tabs below contain data for the number of NR walleye lakes with a specified driving radius of the selected lake in 2020 and 2050"),
                                    textOutput("travelDesc.Text"),
                                    br(),
                                    tabsetPanel(
                                      tabPanel("2020",
                                               br(),
                                               br(),
                                               shinybusy::add_busy_spinner(spin = "fading-circle",timeout=1000,margins = c(500,1000)),
                                               fluidRow(splitLayout(plotOutput("mi30Lakes",click = "plot_clickCost30"),
                                                                    DT::dataTableOutput("mi30DT"))),
                                               fluidRow(splitLayout(plotOutput("mi45Lakes",click = "plot_clickCost45"),
                                                                    DT::dataTableOutput("mi45DT"))),
                                               fluidRow(splitLayout(plotOutput("mi60Lakes",click = "plot_clickCost60"),
                                                                    DT::dataTableOutput("mi60DT")))),
                                      tabPanel("2050",
                                               br(),
                                               br(),
                                               shinybusy::add_busy_spinner(spin = "fading-circle",timeout=1000,margins = c(500,500)),
                                               fluidRow(splitLayout(plotOutput("mi30LakesF",click = "plot_clickCost30F"),
                                                                    DT::dataTableOutput("mi30DTF"))),
                                               fluidRow(splitLayout(plotOutput("mi45LakesF",click = "plot_clickCost45F"),
                                                                    DT::dataTableOutput("mi45DTF"))),
                                               fluidRow(splitLayout(plotOutput("mi60LakesF",click = "plot_clickCost60F"),
                                                                    DT::dataTableOutput("mi60DTF"))))
                                    )),
                           tabPanel("Travel Costs",
                                   fluidRow(column(width = 4, 
                                                   wellPanel(verticalLayout(selectizeInput("stateIn",
                                                                                           "Select State",
                                                                                           state.name,
                                                                                           selected="Wisconsin"),
                                                                            textInput("cityIn","Traveling From:",placeholder = "Stevens Point"),                                                                              
                                                                            numericInput("nearN","Nearest Number of Lakes",value = 10)))),
                                            column(width = 7,offset = 1,wellPanel(checkboxGroupInput("lClass",
                                                                                                     "Select Lake Classes to Display",
                                                                                                     choices = sort(unique(curdb$Final.Lake.Class)),
                                                                                                     selected = "Complex - Cool - Clear",
                                                                                                     inline = T)),
                                                    actionButton("citySubmitButton","Submit",class="btn-sucess"))),
                         
                                    fluidRow(splitLayout(verticalLayout(h3(textOutput("curDistTabName")),DT::dataTableOutput("curTravelData"))),
                                                         verticalLayout(h3(textOutput("futDistTabName")),DT::dataTableOutput("futTravelData"))),
                                   br(),
                                   hr(style="border-top: 1px solid #000000;"),
                                   br(),
                         fluidRow(column(width = 4,
                                         h3(textOutput("lakeCostInfo")),
                                         tableOutput("lakeCost")),
                                  column(width = 6,
                                         h3("Distance to Nearest 'Quality' Walleye Fishery (Regardless of Lake Class)"),
                                         tableOutput("nearQualCost"))))
                         )),
                
                tabPanel("Invasive Species",
                         br(),
                         p("Use the link below to go to the Wisconsin AIS Smart Prevention tool (2.0) and find out the risk of invasion for the specified lake to a number of invasive species."),
                         a("AIS Smart Prevention tool",href="https://uwlimnology.shinyapps.io/AISSmartPrevention2/",target="_blank"),
                         br(),
                         hr(style="border-top: 1px solid #000000;"),
                         br(),
                         fluidRow(column(width=10,offset=1,wellPanel(h3(textOutput("invSppList"))))),
                         br(),
                         br(),
                         fluidRow(column(width=6,align="center",verticalLayout(h3(textOutput("invTableText")),
                                                                               DT::dataTableOutput("ClassInv"))),
                                  column(width=6,align="center",verticalLayout(h3(textOutput("meanInvText")),
                                                                               tableOutput("meanInv")))),
                         br(),
                         br()
                ),
                tabPanel("Summary/Download",
                         br(),
                         htmlOutput("summaryText"),
                         br(),
                         br(),
                         p("This information can be used to decide which of the 3 categories described below best describe the management goals for this lake. This can aid in deciding what management actions related to walleye are most appropriate here."),
                         br(),
                         wellPanel(p("Resist decisions - Resist the effects of climate change, angling effort, etc. on walleye and continue to manage the system for walleye opportunities."),
                                   p("Accept decisions - Do not resist the effects of climate change, angling effort, etc. and allow the system to transition naturally to a new stable configuration."),
                                   p("Direct decisions - Do not resist the effects of climate change, angling effort, etc. and take action to",strong("direct the system"),"towards an alternative configuration that meets your management objectives.")),
                         br(),
                         br(),
                         fluidRow(textOutput("downloadText")),
                         downloadButton("report","Download Report"),
                         br(),
                         br(),
                         br())
              )
    )
  )
)

#### SERVER #####

# this section contains all the code running in the background to produce what the user sees
# Define server logic required to generate table of lake information
server <- function(input, output) {
  countPick=reactive({input$county})
  output$lname <- renderUI({
    selectizeInput(inputId = "lname2",
                   label = "Lake Name",
                   choices = unique(curdb$Lake.Name[curdb$County==countPick()]),
                   options = list(maxOptions=15,
                                  placeholder='search by Lake Name',
                                  closeAfterSelect=T))
  })
  # setting useful reactive variables to use across tabs
  lake=eventReactive(input$submitButton, {curdb$WBIC[which(curdb$County==input$county & curdb$Lake.Name==input$lname2)]})
  lc1=reactive({unique(curdb$Final.Lake.Class[curdb$WBIC==lake()])})
  lc2=reactive({unique(futdb$Final.Lake.Class[futdb$WBIC==lake()])})
  curStoWBICS=reactive({unique(stoSum$wbic[stoSum$wbic%in%curdb$WBIC[curdb$Final.Lake.Class==lc1()]])})
  futStoWBICS=reactive({unique(stoSum$wbic[stoSum$wbic%in%futdb$WBIC[futdb$Final.Lake.Class==lc2()]])})
  curRegWBICS=reactive({unique(regs$wbic[regs$wbic%in%curdb$WBIC[curdb$Final.Lake.Class==lc1()]])})
  futRegWBICS=reactive({unique(regs$wbic[regs$wbic%in%futdb$WBIC[futdb$Final.Lake.Class==lc2()]])})
  curCreWBICS=reactive({unique(crCPE$WBIC[crCPE$WBIC%in%curdb$WBIC[curdb$Final.Lake.Class==lc1()]])})
  futCreWBICS=reactive({unique(crCPE$WBIC[crCPE$WBIC%in%futdb$WBIC[futdb$Final.Lake.Class==lc2()]])})
  curCpeWBICS=reactive({unique(cpe$WBIC[cpe$WBIC%in%curdb$WBIC[curdb$Final.Lake.Class==lc1()]])})
  futCpeWBICS=reactive({unique(cpe$WBIC[cpe$WBIC%in%futdb$WBIC[futdb$Final.Lake.Class==lc2()]])})
  codes=reactive({spk$Fish.Species.Code[spk$Common.Name%in%input$spp]}) # pulling codes for clicked species names
  codes2=reactive({spk$Fish.Species.Code[spk$Common.Name%in%input$vizPlots]}) #pulling codes for creel spp plots to display
  cpeNames=reactive({spk$standCPE[spk$Common.Name%in%input$spp]})#pulling species names from cpe that match input names using crosswalk file
  sfCPW=0.30 # mean cost per walleye (2010-2014) from 5 DNR hatcheries for small fingerlings
  lfCPW=2.53 # mean cost per walleye (2010-2014) from 5 DNR hatcheries for large fingerlings
  otherCPW=0.93 # substitute price per walleye from DNR hatcheries for age classes where data are not available.
  
  stoSum=stoSum[!is.na(stoSum$age.class),] # getting rid of age.class == NA's since they mess up the code and aren't informative
  
  #each of the collapsible sections of code below contain the code necessary to produce the information on each of the parts of the UI defined above
  # Lake class and NR rendering = the main information panels above the tabs
  # regulations rendering = code for the regulations tab and associated sub-tabs
  # stocking summaries = code for the stocking tab
  # Creel Information = code for the creel tab
  # population metrics = code for the population metrics tab and associated sub-tabs
  # Cost information = code for the Alternative Walleye options & travel costs tab and associated sub tabs
  # Invasive Species = code for the invasive species tab
  # Public Report Download = code for the Summary/Download tab where WDNR managers can generate a report that can be handed out to the public
  
  # to better understand the structure of the app, it is recommended that you first collapse all the code sections below using the small dropdown arrow to the left of the section name
  # this will allow you to more cleanly see how the app is set up without the unnecessary detail of the code within each tab.
  
  ##### Lake class and NR rendering ####
  output$lakeConstants <- renderTable({ #table of basic lake information
    
    validate(need(any(curdb$WBIC==lake()), message = paste("No Lake Classification or Recruitment Predictions for WBIC", lake(), sep = " ")))
    
    df=data.frame(Parameter=c("Lake Name","WBIC","Area (acres)","Max.Depth (ft)"), Value=t(curdb[which(curdb$WBIC==lake()),c(2,1,4,6)])[,1])
    colnames(df)=c(" "," ")
    
    df
  })
  
  output$lakeTimeVariable <- renderTable(rownames=T,{ #table of lake information that is projected for 2020 & 2050, basic walleye recruitment and presence/absence predictions
    
    curTV=as.data.frame(curdb[curdb$WBIC==lake(),c(10:13,20,24,26)])
    colnames(curTV)=c("Lake Class", "Temp. Transitional System","Clarity Transitional System","Alternative Classification for 2-Story and Stocked Trout Lakes","Upstream Probability of Adults","Probability of Supporting Adult Walleye","Probability of Successful Natural Recruitment")
    
    futTV=as.data.frame(futdb[futdb$WBIC==lake(),c(14,11:13,16,15)])
    colnames(futTV)=c("Lake Class", "Temp. Transitional System","Clarity Transitional System","Alternative Classification for 2-Story and Stocked Trout Lakes","Probability of Supporting Adult Walleye", "Probability of Successful Natural Recruitment")
    
    df=as.data.frame(t(full_join(curTV,futTV)))
    colnames(df)=c("Current - 2020","Mid-Century - 2050")
    
    df
  })
  output$lakeClassBins <- renderPlot({ #plot comparing selected lake's natural recruitment prediction to the distribution of predictions for its lake class in 2020 and 2050
    cdat=curdb[,c(1,10,26)]
    cdat$time=rep("2020",nrow(cdat))
    fdat=futdb[,c(1,14,15)]
    fdat$time=rep("2050",nrow(fdat))
    
    dat=rbind(cdat,fdat)
    dat$Final.Lake.Class[is.na(dat$Final.Lake.Class)]="Unclassified"
    jitt1=position_nudge(x = .2)
    jitt2=position_nudge(x = -.2)
    
    ggplot()+theme_classic()+
      geom_boxplot(data=dat,aes(x=Final.Lake.Class,y=probability.recruit.1,fill=time))+
      geom_point(aes(x=lc1(),y=cdat$probability.recruit.1[cdat$WBIC==lake()]),color="#009E73",shape=8,size =4,position = jitt2)+
      geom_point(aes(x=lc2(),y=fdat$probability.recruit.1[fdat$WBIC==lake()]),color="#0072B2",shape=8,size =4,position = jitt1)+
      theme(axis.text.x = element_text(angle=45,hjust=1),
            legend.position = "bottom",legend.title = element_blank(),
            axis.title = element_text(size=15),
            axis.text = element_text(size=15),
            legend.text = element_text(size=15))+
      labs(x=element_blank(),y="Probability of \n Sucessful Natural Recruitment")+
      scale_fill_manual(values = c("#009E73","#0072B2"))
  })
  output$lakeClassPlotText <- renderText({ #text describing the plot coded above, this is rendered alongside the plot for the benefit of the app user
    
    paste( "Lake class probabilities of sucessful natural recruitment are the box-and-whisker plots. ", curdb$Lake.Name[curdb$WBIC==lake()], " specific natural recruitment predictions are the star points (green=2020, blue=2050).", sep = "")
    
  })
  output$lakeShape <- renderPlot({
    wbicTomap=hdro[hdro$WATERBOD_2==lake(),]
    wbicTomap$lakeFill=rep("#0072B2",nrow(wbicTomap))
    wbicTomap$latitude=curdb$Latitude[curdb$WBIC==lake()]
    wbicTomap$longitude=curdb$Longitude[curdb$WBIC==lake()]
    ggplot()+theme_void()+
      geom_sf(data = wbicTomap$geometry, fill=wbicTomap$lakeFill)+
      annotation_scale(location="br", width_hint=0.5,text_cex = 2,unit_category="imperial")+
      annotation_north_arrow(location="br", which_north="true",pad_y = unit(1,"cm"),style = north_arrow_orienteering(text_size = 30,line_width = 3))
  })
  
  ##### Regulations rendering ####
  
  
  output$regTableName <- renderText({paste("2020 Fishing regulations for",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")}) # generating table name for regs table
  
  output$curRegs <- renderTable({ # pulling regs for the selected lake and creating a table that easy to read
    
    regs$functional.start.date=format(regs$functional.start.date, "%Y-%m-%d")
    display=regs[which(regs$wbic==lake() & regs$species%in%spk$regsNames[spk$Common.Name%in%sppChoices]),c(5,7,18,20,14,15,21)]
    colnames(display)=c("Species","Reg. Source","Functional Start Date","Reg. Type Description","Gear Restriction","Other Restrictions","Additional Info")
    display[order(display$Species),]
    
  })
  output$curClassRegs <- renderText({paste("Fishing regulations for 2020 lake class:",lc1(),sep = " ")}) # label for the distribution of regs for the lake class of the selected lake
  output$lakeClassRegsText <- renderText("Click on the bars (or the space above them) below to see which lakes are in the given regulation source; a data table containing the regulation information for the lakes with that regualtion source will populate on the right. Each species shown below has its own table. Add or drop columns of regulation information to display using the check boxes (this will change columns for all tables being displayed). Data tables can be sorted and searched as well.") #explainer text for the lake class regs plots
  
  # current lake class
  # all species are shown instead of having managers pick species to show because all of these are important to managers and there are relatively few species to show
  # these plots are clickable so the user can learn more about the information on the plot through data tables that appear next the plot when the plot is clicked.
  output$curWLY <- renderPlot({ # lake class regs distribution for walleye
    
    datCWLY=regs[regs$wbic%in%curRegWBICS() & regs$species=="walleye_sauger_and_hybrids",]
    datCWLY$reg.source=as.factor(datCWLY$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCWLY,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Walleye")
  })
  output$curLMB <- renderPlot({# lake class regs distribution for largemouth bass
    
    datCLMB=regs[regs$wbic%in%curRegWBICS() & regs$species=="largemouth_bass",]
    datCLMB$reg.source=as.factor(datCLMB$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCLMB,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Largemouth Bass")
  })
  output$curSMB <- renderPlot({# lake class regs distribution for smallmouth bass
    
    datCSMB=regs[regs$wbic%in%curRegWBICS() & regs$species=="smallmouth_bass",]
    datCSMB$reg.source=as.factor(datCSMB$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCSMB,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Smallmouth Bass")
  })
  output$curLMB_SMB <- renderPlot({# lake class regs distribution for largemouth and smallmouth combined becuase in some systems they are managed as a group
    
    datCLMB_SMB=regs[regs$wbic%in%curRegWBICS() & regs$species=="largemouth_bass_and_smallmouth_bass",]
    datCLMB_SMB$reg.source=as.factor(datCLMB_SMB$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCLMB_SMB,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Largemouth and Smallmouth Bass")
  })
  output$curMSK <- renderPlot({# lake class regs distribution for muskies
    
    datCMSK=regs[regs$wbic%in%curRegWBICS() & regs$species=="muskellunge_and_hybrids",]
    datCMSK$reg.source=as.factor(datCMSK$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCMSK,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Muskellunge and hybrids")
  })
  output$curNPK <- renderPlot({# lake class regs distribution for northern pike
    
    datCNPK=regs[regs$wbic%in%curRegWBICS() & regs$species=="northern_pike",]
    datCNPK$reg.source=as.factor(datCNPK$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCNPK,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Northern Pike")
  })
  output$curPanfish <- renderPlot({# lake class regs distribution for panfish (complex of several centrarchid species plus yellow perch, see species key code above for full definition)
    
    datCPanfish=regs[regs$wbic%in%curRegWBICS() & regs$species=="panfish",]
    datCPanfish$reg.source=as.factor(datCPanfish$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCPanfish,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Panfish")
  })
  output$curCrappies <- renderPlot({# lake class regs distribution for crappies (black crappie is the dominant species throughout much of the state but white crappie occasionally occurr, they are managed together as 'crappies')
    
    datCrappies=regs[regs$wbic%in%curRegWBICS() & regs$species=="crappies",]
    datCrappies$reg.source=as.factor(datCrappies$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCrappies,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Crappies")
  })
  
  output$curWLYDT <- DT::renderDataTable({ # clickable data table to go with walleye plot
    if(is.null(input$plot_clickCWLY$x)){return()}else{
      datCWLY=regs[regs$wbic%in%curRegWBICS() & regs$species=="walleye_sauger_and_hybrids",]
      datCWLY$reg.source=as.factor(datCWLY$reg.source)
      showRows=round(input$plot_clickCWLY$x) == as.numeric(datCWLY$reg.source)
      DT::datatable(datCWLY[showRows,input$showCols],rownames = F)
    }
  })
  output$curLMBDT <- DT::renderDataTable({# clickable data table to go with largemouth bass plot
    if(is.null(input$plot_clickCLMB$x)){return()}else{
      datCLMB=regs[regs$wbic%in%curRegWBICS() & regs$species=="largemouth_bass",]
      datCLMB$reg.source=as.factor(datCLMB$reg.source)
      showRows=round(input$plot_clickCLMB$x) == as.numeric(datCLMB$reg.source)
      DT::datatable(datCLMB[showRows,input$showCols],rownames = F)
    }
  })
  output$curSMBDT <- DT::renderDataTable({# clickable data table to go with smallmouth bass plot
    if(is.null(input$plot_clickCSMB$x)){return()}else{
      datCSMB=regs[regs$wbic%in%curRegWBICS() & regs$species=="smallmouth_bass",]
      datCSMB$reg.source=as.factor(datCSMB$reg.source)
      showRows=round(input$plot_clickCSMB$x) == as.numeric(datCSMB$reg.source)
      DT::datatable(datCSMB[showRows,input$showCols],rownames = F)
    }
  })
  output$curLMB_SMBDT <- DT::renderDataTable({# clickable data table to go with largemouth and smallmouth combined plot
    if(is.null(input$plot_clickCLMB_SMB$x)){return()}else{
      datCLMB_SMB=regs[regs$wbic%in%curRegWBICS() & regs$species=="largemouth_bass_and_smallmouth_bass",]
      datCLMB_SMB$reg.source=as.factor(datCLMB_SMB$reg.source)
      showRows=round(input$plot_clickCLMB_SMB$x) == as.numeric(datCLMB_SMB$reg.source)
      DT::datatable(datCLMB_SMB[showRows,input$showCols],rownames = F)
    }
  })
  output$curMSKDT <- DT::renderDataTable({# clickable data table to go with musky plot
    if(is.null(input$plot_clickCMSK$x)){return()}else{
      datCMSK=regs[regs$wbic%in%curRegWBICS() & regs$species=="muskellunge_and_hybrids",]
      datCMSK$reg.source=as.factor(datCMSK$reg.source)
      showRows=round(input$plot_clickCMSK$x) == as.numeric(datCMSK$reg.source)
      DT::datatable(datCMSK[showRows,input$showCols],rownames = F)
    }
  })
  output$curNPKDT <- DT::renderDataTable({# clickable data table to go with northern pike plot
    if(is.null(input$plot_clickCNPK$x)){return()}else{
      datCNPK=regs[regs$wbic%in%curRegWBICS() & regs$species=="northern_pike",]
      datCNPK$reg.source=as.factor(datCNPK$reg.source)
      showRows=round(input$plot_clickCNPK$x) == as.numeric(datCNPK$reg.source)
      DT::datatable(datCNPK[showRows,input$showCols],rownames = F)
    }
  })
  output$curPanfishDT <- DT::renderDataTable({# clickable data table to go with panfish plot
    if(is.null(input$plot_clickCPan$x)){return()}else{
      datCPanfish=regs[regs$wbic%in%curRegWBICS() & regs$species=="panfish",]
      datCPanfish$reg.source=as.factor(datCPanfish$reg.source)
      showRows=round(input$plot_clickCPan$x) == as.numeric(datCPanfish$reg.source)
      DT::datatable(datCPanfish[showRows,input$showCols],rownames = F)
    }
  })
  output$curCrappiesDT <- DT::renderDataTable({# clickable data table to go with crappie plot
    if(is.null(input$plot_clickCCra$x)){return()}else{
      datCCrappies=regs[regs$wbic%in%curRegWBICS() & regs$species=="crappies",]
      datCCrappies$reg.source=as.factor(datCCrappies$reg.source)
      showRows=round(input$plot_clickCCra$x) == as.numeric(datCCrappies$reg.source)
      DT::datatable(datCCrappies[showRows,input$showCols],rownames = F)
    }
  })
  
  # future lake class
  # all species are shown instead of having managers pick species to show because all of these are important to managers and there are relatively few species to show
  # these plots are clickable so the user can learn more about the information on the plot through data tables that appear next the plot when the plot is clicked.
  
  output$futClassRegs <- renderText({paste("Fishing regulations for 2050 lake class:",lc2(),sep = " ")})
  output$futWLY <- renderPlot({# lake class regs distribution for walleye
    
    datCWLY=regs[regs$wbic%in%futRegWBICS() & regs$species=="walleye_sauger_and_hybrids",]
    datCWLY$reg.source=as.factor(datCWLY$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCWLY,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Walleye")
  })
  output$futLMB <- renderPlot({# lake class regs distribution for largemouth bass
    
    datCLMB=regs[regs$wbic%in%futRegWBICS() & regs$species=="largemouth_bass",]
    datCLMB$reg.source=as.factor(datCLMB$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCLMB,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Largemouth Bass")
  })
  output$futSMB <- renderPlot({# lake class regs distribution for smallmouth bass
    
    datCSMB=regs[regs$wbic%in%futRegWBICS() & regs$species=="smallmouth_bass",]
    datCSMB$reg.source=as.factor(datCSMB$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCSMB,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Smallmouth Bass")
  })
  output$futLMB_SMB <- renderPlot({# lake class regs distribution for largemouth and smallmouth combined becuase in some systems they are managed as a group
    
    datCLMB_SMB=regs[regs$wbic%in%futRegWBICS() & regs$species=="largemouth_bass_and_smallmouth_bass",]
    datCLMB_SMB$reg.source=as.factor(datCLMB_SMB$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCLMB_SMB,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Largemouth and Smallmouth Bass")
  })
  output$futMSK <- renderPlot({# lake class regs distribution for muskies
    
    datCMSK=regs[regs$wbic%in%futRegWBICS() & regs$species=="muskellunge_and_hybrids",]
    datCMSK$reg.source=as.factor(datCMSK$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCMSK,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Muskellunge and hybrids")
  })
  output$futNPK <- renderPlot({# lake class regs distribution for northern pike
    
    datCNPK=regs[regs$wbic%in%futRegWBICS() & regs$species=="northern_pike",]
    datCNPK$reg.source=as.factor(datCNPK$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCNPK,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Northern Pike")
  })
  output$futPanfish <- renderPlot({ # lake class regs distribution for panfish (complex of several centrarchid species plus yellow perch, see species key code above for full definition)
    
    datCPanfish=regs[regs$wbic%in%futRegWBICS() & regs$species=="panfish",]
    datCPanfish$reg.source=as.factor(datCPanfish$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCPanfish,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Panfish")
  })
  output$futCrappies <- renderPlot({# lake class regs distribution for crappies (black crappie is the dominant species throughout much of the state but white crappie occasionally occurr, they are managed together as 'crappies')
    
    datCrappies=regs[regs$wbic%in%futRegWBICS() & regs$species=="crappies",]
    datCrappies$reg.source=as.factor(datCrappies$reg.source)
    ggplot()+theme_classic()+
      geom_bar(data=datCrappies,aes(x=reg.source))+
      theme(text = element_text(size = 17))+labs(x="Regulation Source",y="Frequency",title = "Crappies")
  })
  
  output$futWLYDT <- DT::renderDataTable({# clickable data table to go with walleye plot
    if(is.null(input$plot_clickFWLY$x)){return()}else{
      datCWLY=regs[regs$wbic%in%futRegWBICS() & regs$species=="walleye_sauger_and_hybrids",]
      datCWLY$reg.source=as.factor(datCWLY$reg.source)
      showRows=round(input$plot_clickFWLY$x) == as.numeric(datCWLY$reg.source)
      DT::datatable(datCWLY[showRows,input$showCols],rownames = F)
    }
  })
  output$futLMBDT <- DT::renderDataTable({# clickable data table to go with largemouth bass plot
    if(is.null(input$plot_clickFLMB$x)){return()}else{
      datCLMB=regs[regs$wbic%in%futRegWBICS() & regs$species=="largemouth_bass",]
      datCLMB$reg.source=as.factor(datCLMB$reg.source)
      showRows=round(input$plot_clickFLMB$x) == as.numeric(datCLMB$reg.source)
      DT::datatable(datCLMB[showRows,input$showCols],rownames = F)
    }
  })
  output$futSMBDT <- DT::renderDataTable({# clickable data table to go with smallmouth bass plot
    if(is.null(input$plot_clickFSMB$x)){return()}else{
      datCSMB=regs[regs$wbic%in%futRegWBICS() & regs$species=="smallmouth_bass",]
      datCSMB$reg.source=as.factor(datCSMB$reg.source)
      showRows=round(input$plot_clickFSMB$x) == as.numeric(datCSMB$reg.source)
      DT::datatable(datCSMB[showRows,input$showCols],rownames = F)
    }
  })
  output$futLMB_SMBDT <- DT::renderDataTable({# clickable data table to go with largemouth and smallmouth combined plot
    if(is.null(input$plot_clickFLMB_SMB$x)){return()}else{
      datCLMB_SMB=regs[regs$wbic%in%futRegWBICS() & regs$species=="largemouth_bass_and_smallmouth_bass",]
      datCLMB_SMB$reg.source=as.factor(datCLMB_SMB$reg.source)
      showRows=round(input$plot_clickFLMB_SMB$x) == as.numeric(datCLMB_SMB$reg.source)
      DT::datatable(datCLMB_SMB[showRows,input$showCols],rownames = F)
    }
  })
  output$futMSKDT <- DT::renderDataTable({# clickable data table to go with musky plot
    if(is.null(input$plot_clickFMSK$x)){return()}else{
      datCMSK=regs[regs$wbic%in%futRegWBICS() & regs$species=="muskellunge_and_hybrids",]
      datCMSK$reg.source=as.factor(datCMSK$reg.source)
      showRows=round(input$plot_clickFMSK$x) == as.numeric(datCMSK$reg.source)
      DT::datatable(datCMSK[showRows,input$showCols],rownames = F)
    }
  })
  output$futNPKDT <- DT::renderDataTable({# clickable data table to go with northern pike plot
    if(is.null(input$plot_clickFNPK$x)){return()}else{
      datCNPK=regs[regs$wbic%in%futRegWBICS() & regs$species=="northern_pike",]
      datCNPK$reg.source=as.factor(datCNPK$reg.source)
      showRows=round(input$plot_clickFNPK$x) == as.numeric(datCNPK$reg.source)
      DT::datatable(datCNPK[showRows,input$showCols],rownames = F)
    }
  })
  output$futPanfishDT <- DT::renderDataTable({# clickable data table to go with panfish plot
    if(is.null(input$plot_clickFPan$x)){return()}else{
      datCPanfish=regs[regs$wbic%in%futRegWBICS() & regs$species=="panfish",]
      datCPanfish$reg.source=as.factor(datCPanfish$reg.source)
      showRows=round(input$plot_clickFPan$x) == as.numeric(datCPanfish$reg.source)
      DT::datatable(datCPanfish[showRows,input$showCols],rownames = F)
    }
  })
  output$futCrappiesDT <- DT::renderDataTable({# clickable data table to go with crappie plot
    if(is.null(input$plot_clickFCra$x)){return()}else{
      datCCrappies=regs[regs$wbic%in%futRegWBICS() & regs$species=="crappies",]
      datCCrappies$reg.source=as.factor(datCCrappies$reg.source)
      showRows=round(input$plot_clickFCra$x) == as.numeric(datCCrappies$reg.source)
      DT::datatable(datCCrappies[showRows,input$showCols],rownames = F)
    }
  })  
  
  ##### Stocking summaries ####
  
  # rendering titles and text for the user to see alongside the stocking data plots and tables
  output$stoHistTitle <- renderText(paste("Stocking Histories for",curdb$Lake.Name[curdb$WBIC==lake()]," and its 2020 and 2050 Lake Class",sep = " "))
  output$lakeStoInfoTitle <- renderText(paste("Stocking History for", curdb$Lake.Name[curdb$WBIC==lake()]),sep = " ")
  output$lakeStoCostsTitle <- renderText(paste("Stocking Cost by Age-Class for ",curdb$Lake.Name[curdb$WBIC==lake()],sep = " "))
  output$curStoCostsTitle <- renderText(paste("Stocking Cost by Age-Class for ",curdb$Final.Lake.Class[curdb$WBIC==lake()],sep = " "))
  output$futStoCostsTitle <- renderText(paste("Stocking Cost by Age-Class for ",futdb$Final.Lake.Class[futdb$WBIC==lake()],sep = " "))
  output$stoDescriptiveText <- renderText("Chose walleye stocking ages classes to display below. The selected lake, and mean values for its current and future lake classes are displayed (left) as well as boxplots of the same data (right). When no stocking data exists for a given lake or lake class it won't be displayed on the plot.")
  output$stoSumReport <- renderText({
    
    validate(need(any(stoSum$wbic==lake() & stoSum$age.class==input$ageClass), message = paste("No stocking data available for selected age classes in",curdb$Lake.Name[curdb$WBIC==lake()],sep = " "))) #generate a warnings for missing data as not every lake has been stocked and not every stocked lake is stocked every year
    
  })
  
  output$stoCostDescrp <- renderText("Stocking costs per lake summed across all stocking events for the selected lake, and mean values for its current and future lake classes.")
  
  output$lakeStoCosts <- renderTable({ # table with stocking cost information for the selected lake
    
    validate(need(any(stoSum$wbic==lake()), message = paste("No stocking summaries available for ",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")))
    
    lakeHistCost = stoSum%>%
      filter(stoSum$wbic==lake())%>%
      group_by(age.class)%>%
      filter(age.class%in%c("fingerling","large_fingerling","small_fingerling"))%>%
      summarise(nStocked = sum(number.fish.stocked,na.rm = T),
                nYears=n_distinct(year))
    lakeHistCost$cpw=rep(otherCPW,nrow(lakeHistCost))
    for(i in 1:nrow(lakeHistCost)){
      if(lakeHistCost$age.class[i]=="small_fingerling"){lakeHistCost$cpw[i]=sfCPW}else{
        if(lakeHistCost$age.class[i]=="large_fingerling"){lakeHistCost$cpw[i]=lfCPW}
      }
    }
    lakeHistCost$annualTotal=(lakeHistCost$nStocked*lakeHistCost$cpw)/lakeHistCost$nYears
    colnames(lakeHistCost)=c("Age Class","Number Stocked","Years Stocked","Cost-per-fish","Annual Total")
    lakeHistCost
  })
  
  output$lakeCumuStoCost <- renderText({ # table with stocking cost information summed across all stocking events for the selected lake
    
    validate(need(any(stoSum$wbic==lake()), message = paste("No stocking summaries available for ",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")))
    
    lakeHistCost = stoSum%>%
      filter(stoSum$wbic==lake())%>%
      group_by(age.class)%>%
      filter(age.class%in%c("fingerling","large_fingerling","small_fingerling"))%>%
      summarise(nStocked = sum(number.fish.stocked,na.rm = T),
                nYears=n_distinct(year))
    lakeHistCost$cpw=rep(otherCPW,nrow(lakeHistCost))
    for(i in 1:nrow(lakeHistCost)){
      if(lakeHistCost$age.class[i]=="small_fingerling"){lakeHistCost$cpw[i]=sfCPW}else{
        if(lakeHistCost$age.class[i]=="large_fingerling"){lakeHistCost$cpw[i]=lfCPW}
      }
    }
    lakeHistCost$annualTotal=(lakeHistCost$nStocked*lakeHistCost$cpw)/lakeHistCost$nYears
    
    print(paste("Total Annual Stocking Expenditure for", curdb$Lake.Name[curdb$WBIC==lake()],":",paste("$",round(sum(lakeHistCost$annualTotal,na.rm = T),digits = 2),sep = ""),sep = " "))
  })
  
  output$curClassCosts <- renderTable({ # table with average stocking costs per year for the selected lake's lake class
    curClassCost = stoSum%>%
      filter(stoSum$wbic%in%curStoWBICS())%>%
      group_by(wbic,age.class)%>%
      filter(age.class%in%c("fingerling","large_fingerling","small_fingerling"))%>%
      summarise(nStocked = sum(number.fish.stocked,na.rm = T),
                nYears=n_distinct(year))%>%
      mutate(stoPerYear=nStocked/nYears)%>%
      group_by(age.class)%>%
      summarise(meanNStockedYr=mean(stoPerYear))
    
    curClassCost$cpw=rep(otherCPW,nrow(curClassCost))
    for(i in 1:nrow(curClassCost)){
      if(curClassCost$age.class[i]=="small_fingerling"){curClassCost$cpw[i]=sfCPW}else{
        if(curClassCost$age.class[i]=="large_fingerling"){curClassCost$cpw[i]=lfCPW}
      }
    }
    curClassCost$annualTotal=curClassCost$meanNStockedYr*curClassCost$cpw
    colnames(curClassCost)=c("Age Class","Mean Number Stocked per WBIC","Cost-per-fish","Annual Total")
    curClassCost
  })
  
  output$curCumuStoCost <- renderText({ # table with average stocking cost summed across the average number of stocking events for the selected lake's lake class
    curClassCost = stoSum%>%
      filter(stoSum$wbic%in%curStoWBICS())%>%
      group_by(wbic,age.class)%>%
      filter(age.class%in%c("fingerling","large_fingerling","small_fingerling"))%>%
      summarise(nStocked = sum(number.fish.stocked,na.rm = T),
                nYears=n_distinct(year))%>%
      mutate(stoPerYear=nStocked/nYears)%>%
      group_by(age.class)%>%
      summarise(meanNStockedYr=mean(stoPerYear))
    
    curClassCost$cpw=rep(otherCPW,nrow(curClassCost))
    for(i in 1:nrow(curClassCost)){
      if(curClassCost$age.class[i]=="small_fingerling"){curClassCost$cpw[i]=sfCPW}else{
        if(curClassCost$age.class[i]=="large_fingerling"){curClassCost$cpw[i]=lfCPW}
      }
    }
    curClassCost$annualTotal=curClassCost$meanNStockedYr*curClassCost$cpw
    
    print(paste("Mean Total Annual Stocking Expenditure for a", curdb$Final.Lake.Class[curdb$WBIC==lake()],"Lake",":",paste("$",round(sum(curClassCost$annualTotal,na.rm = T),digits = 2),sep = ""),sep = " "))
  })
  
  output$futClassCosts <- renderTable({ # table with future average annual stocking costs for the selected lake's future lake class
    futClassCost = stoSum%>%
      filter(stoSum$wbic%in%futStoWBICS())%>%
      group_by(wbic,age.class)%>%
      filter(age.class%in%c("fingerling","large_fingerling","small_fingerling"))%>%
      summarise(nStocked = sum(number.fish.stocked,na.rm = T),
                nYears=n_distinct(year))%>%
      mutate(stoPerYear=nStocked/nYears)%>%
      group_by(age.class)%>%
      summarise(meanNStockedYr=mean(stoPerYear))
    
    futClassCost$cpw=rep(otherCPW,nrow(futClassCost))
    for(i in 1:nrow(futClassCost)){
      if(futClassCost$age.class[i]=="small_fingerling"){futClassCost$cpw[i]=sfCPW}else{
        if(futClassCost$age.class[i]=="large_fingerling"){futClassCost$cpw[i]=lfCPW}
      }
    }
    futClassCost$annualTotal=futClassCost$meanNStockedYr*futClassCost$cpw
    colnames(futClassCost)=c("Age Class","Mean Number Stocked per WBIC","Cost-per-fish","Annual Total")
    futClassCost
  })
  
  output$futCumuStoCost <- renderText({ # table with average stocking cost summed across the average number of stocking events for the selected lake's future lake class
    futClassCost = stoSum%>%
      filter(stoSum$wbic%in%futStoWBICS())%>%
      group_by(wbic,age.class)%>%
      filter(age.class%in%c("fingerling","large_fingerling","small_fingerling"))%>%
      summarise(nStocked = sum(number.fish.stocked,na.rm = T),
                nYears=n_distinct(year))%>%
      mutate(stoPerYear=nStocked/nYears)%>%
      group_by(age.class)%>%
      summarise(meanNStockedYr=mean(stoPerYear))
    
    futClassCost$cpw=rep(otherCPW,nrow(futClassCost))
    for(i in 1:nrow(futClassCost)){
      if(futClassCost$age.class[i]=="small_fingerling"){futClassCost$cpw[i]=sfCPW}else{
        if(futClassCost$age.class[i]=="large_fingerling"){futClassCost$cpw[i]=lfCPW}
      }
    }
    futClassCost$annualTotal=futClassCost$meanNStockedYr*futClassCost$cpw
    
    print(paste("Mean Total Annual Stocking Expenditure for a", futdb$Final.Lake.Class[futdb$WBIC==lake()],"lake",":",paste("$",round(sum(futClassCost$annualTotal,na.rm = T),digits = 2),sep = ""),sep = " "))
    
  })
  
  output$meanStoSumPlot <- renderPlot({ # plot of stocking density through time for the selected lake and it's current and futue lake class means
    
    pdL=stoSum[stoSum$wbic==lake() & stoSum$age.class%in%input$ageClass,]
    pdL$stockedAcre=pdL$number.fish.stocked/curdb$Area..acres.[curdb$WBIC==lake()]
    pdL$label=as.factor(rep(curdb$Lake.Name[curdb$WBIC==lake()],nrow(pdL)))
    pdL=pdL[,c("year","age.class","stockedAcre","label")]
    pdL$sd=rep(NA,nrow(pdL))
    pdL=pdL[,c("year","age.class","stockedAcre","sd","label")]
    
    pdC=stoSum%>%
      filter(stoSum$wbic%in%curStoWBICS() & stoSum$age.class%in%input$ageClass)%>%
      left_join(curdb, by=c("wbic"="WBIC"))%>%
      group_by(wbic,year, age.class,Area..acres.)%>%
      summarise(meanNumberStocked=mean(number.fish.stocked,na.rm=T),
                sdNumberStocked=sd(number.fish.stocked,na.rm = T))%>%
      mutate(meanStoPerLake_perAcre=meanNumberStocked/Area..acres.,
             sdStoPerLake_perAcre=sdNumberStocked/Area..acres.)%>%
      group_by(year, age.class)%>%
      summarize(meanNStoDensPerWBIC=mean(meanStoPerLake_perAcre,na.rm=T),
                sdNStoDensPerWBIC=sd(meanStoPerLake_perAcre,na.rm = T))
    pdC$label=as.factor(rep("Current Class",nrow(pdC)))
    colnames(pdC)=colnames(pdL)
    pdF=stoSum%>%
      filter(stoSum$wbic%in%futStoWBICS() & stoSum$age.class%in%input$ageClass)%>%
      left_join(futdb, by=c("wbic"="WBIC"))%>%
      group_by(wbic,year, age.class,Area..acres.)%>%
      summarise(meanNumberStocked=mean(number.fish.stocked,na.rm=T),
                sdNumberStocked=sd(number.fish.stocked,na.rm = T))%>%
      mutate(meanStoPerLake_perAcre=meanNumberStocked/Area..acres.)%>%
      group_by(year, age.class)%>%
      summarize(meanNStoDensPerWBIC=mean(meanStoPerLake_perAcre,na.rm=T),
                sdNStoDensPerWBIC=sd(meanStoPerLake_perAcre,na.rm = T))
    pdF$label=as.factor(rep("Future Class",nrow(pdF)))
    colnames(pdF)=colnames(pdL)
    pd=rbind(pdL,pdC,pdF)
    ord=c(unique(pdL$label),unique(pdC$label),unique(pdF$label))
    pd$label=factor(pd$label,levels = ord)
    
    #posD=position_dodge(0.3)
    
    ggplot()+theme_classic()+
      geom_line(data=pd,aes(x=year,y=stockedAcre, color=label))+
      geom_pointrange(data=pd,aes(x=year,y=stockedAcre,ymin=stockedAcre-sd, ymax=stockedAcre+sd, color=label,size=label))+
      facet_wrap(~age.class, scales = "free_y")+
      labs(x="Year",y="Mean Stocking Density per Acre")+
      coord_cartesian(xlim = c(1972,2021),ylim=c(0,NA))+theme(text = element_text(size = 17),legend.position = "bottom",legend.title = element_blank())+
      scale_size_manual(values = c(2,1.75,1))+guides(color=guide_legend(override.aes = list(size=1)))+
      scale_color_manual(values = c("#999999","#009E73","#0072B2"))
    
    
  })
  
  output$stoBoxWhisk <- renderPlot({ # plot of stocking density distribution for the selected lake and it's current and futue lake class means
    
    pdL=stoSum[stoSum$wbic==lake() & stoSum$age.class%in%input$ageClass,]
    pdL$stockedAcre=pdL$number.fish.stocked/curdb$Area..acres.[curdb$WBIC==lake()]
    pdL$label=as.factor(rep(curdb$Lake.Name[curdb$WBIC==lake()],nrow(pdL)))
    pdL$sd=rep(NA,nrow(pdL))
    pdL=pdL[,c("year","age.class","stockedAcre","sd","label")]
    
    pdC=stoSum%>%
      filter(stoSum$wbic%in%curStoWBICS() & stoSum$age.class%in%input$ageClass)%>%
      left_join(curdb, by=c("wbic"="WBIC"))%>%
      group_by(wbic,year, age.class,Area..acres.)%>%
      summarise(meanNumberStocked=mean(number.fish.stocked,na.rm=T),
                sdNumberStocked=sd(number.fish.stocked,na.rm = T))%>%
      mutate(meanStoPerLake_perAcre=meanNumberStocked/Area..acres.,
             sdStoPerLake_perAcre=sdNumberStocked/Area..acres.)%>%
      group_by(year, age.class)%>%
      summarize(meanNStoDensPerWBIC=mean(meanStoPerLake_perAcre,na.rm=T),
                sdNStoDensPerWBIC=sd(meanStoPerLake_perAcre,na.rm = T))
    pdC$label=as.factor(rep("Current Class",nrow(pdC)))
    colnames(pdC)=colnames(pdL)
    pdF=stoSum%>%
      filter(stoSum$wbic%in%futStoWBICS() & stoSum$age.class%in%input$ageClass)%>%
      left_join(futdb, by=c("wbic"="WBIC"))%>%
      group_by(wbic,year, age.class,Area..acres.)%>%
      summarise(meanNumberStocked=mean(number.fish.stocked,na.rm=T),
                sdNumberStocked=sd(number.fish.stocked,na.rm = T))%>%
      mutate(meanStoPerLake_perAcre=meanNumberStocked/Area..acres.)%>%
      group_by(year, age.class)%>%
      summarize(meanNStoDensPerWBIC=mean(meanStoPerLake_perAcre,na.rm=T),
                sdNStoDensPerWBIC=sd(meanStoPerLake_perAcre,na.rm = T))
    pdF$label=as.factor(rep("Future Class",nrow(pdF)))
    colnames(pdF)=colnames(pdL)
    pd=rbind(pdL,pdC,pdF)
    ord=c(unique(pdL$label),unique(pdC$label),unique(pdF$label))
    pd$label=factor(pd$label,levels = ord)
    
    lakePoints=(pd[!(pd$label%in%c("Current Class","Future Class")),])
    lakeMeanPoints=lakePoints%>%
      group_by(age.class,label)%>%
      summarize(meanStoAc=mean(stockedAcre,na.rm = T))
    lakeMeanPoints=rbind(lakeMeanPoints,lakeMeanPoints)
    lakeMeanPoints$plotLab=c(rep("Current Class",nrow(lakeMeanPoints)/2),rep("Future Class",nrow(lakeMeanPoints)/2))
    allLakePoints=data.frame(label=c(rep("Current Class",nrow(lakePoints)),rep("Future Class",nrow(lakePoints))),
                             value=c(lakePoints$stockedAcre,lakePoints$stockedAcre),
                             age.class=c(lakePoints$age.class,lakePoints$age.class))
    #posJ=position_jitter(0.1)
    ggplot()+theme_classic()+
      geom_boxplot(data = pd,aes(x=label,y=stockedAcre,fill=label))+
      labs(y="Stocking Density per Acre",x=element_blank())+facet_wrap(~age.class,scales = "free_y")+
      theme(text = element_text(size = 17),axis.text.x = element_text(angle = 45,hjust=1))+
      scale_fill_manual(values = c("#999999","#009E73","#0072B2"))
  })
  
  ##### Creel Information #####
  
  # code to create the title and descriptive text for the creel plots and tables
  output$creDescText <- renderText("Creel information is displayed for the selected lake and it's 2020 and 2050 lake class if avialable. At the bottom of each plot there is more plot specific information about the data presented.")
  output$lakeCreelInfoTitle <- renderText({paste("Creel Information for",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")})
  output$creLenPlotText <- renderText({
    "Relative frequency of the observed fish lengths in creel records for each species and group. Using relative frequency instead of the acutaly frequency allows for more direct comparision between the distribution of catch lengths for one lake compared the distribution of catch lengths for all the other lakes in it's current/future lake class."
  })
  output$creBagPlotText <- renderText({
    "The number of harvested  fish of each species at the trip level. Each point corresopnds to the number of fish harvest per angling trip and the number of anglers on that trip. The horizonal red line denotes the bag limit for the selected lake."
  })
  output$creBoxWhiskText <- renderText({
    "Boxplots of the selected lake, it's 2020 lake class, and 2050 lake class catch-per-unit effort distributions at the trip level for each selected species. "
  })
  
  output$creelSPP <- renderUI({ # code to allow the use to decide what species creel data they want to see
    checkboxGroupInput("vizPlots","Choose Species Plots to desplay",choices=input$spp,selected = input$spp,inline = T)
  })
  output$crDataWarn <- renderText({ # code to generate warning if not creel data exists for the selected lake.
    validate(need(any(crCPE$WBIC==lake() & crCPE$Fish.Species.Code%in%codes()), message = paste("No creel surveys available for the selected species in",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")))
  })
  
  output$lakeCreelLengths <- renderPlot({ # plot comparing the length frequency of the selected species for the selected lake and its current and future lake class means
    
    validate(need(any(crLen$WBIC%in%curCreWBICS() & crLen$WBIC%in%futCreWBICS() & crLen$Species.Code%in%codes()), message = paste("No creel catch length info available for the selected species in 2020:",curdb$Final.Lake.Class[curdb$WBIC==lake()],"and 2050:",futdb$Final.Lake.Class[futdb$WBIC==lake()],"lake classes",sep = " ")))
    
    plL=crLen%>%
      filter(crLen$WBIC==lake() & crLen$Species.Code%in%codes2())%>%
      select(Species.Code,Fish.Length)
    
    plC=crLen%>%
      filter(crLen$WBIC%in%curCreWBICS() & crLen$Species.Code%in%codes2())%>%
      select(Species.Code,Fish.Length)
    
    plF=crLen%>%
      filter(crLen$WBIC%in%futCreWBICS() & crLen$Species.Code%in%codes2())%>%
      select(Species.Code,Fish.Length)
    
    plL$label=rep(curdb$Lake.Name[curdb$WBIC==lake()],nrow(plL))
    plC$label=rep("Current Class",nrow(plC))
    plF$label=rep("Future Class",nrow(plF))
    ord=c(unique(plL$label),unique(plC$label),unique(plF$label))
    pl=rbind(plL,plC,plF)
    pl$label=factor(pl$label,levels = ord)
    pl=inner_join(pl,spk,by=c("Species.Code"="Fish.Species.Code"))%>%
      mutate(label=fct_relevel(label,levels=curdb$Lake.Name[curdb$WBIC==lake()],"Current Class","Future Class"))
    pl$Fish.Length=as.numeric(pl$Fish.Length)
    ggplot(data = pl,aes(x=Fish.Length,y=after_stat(density), fill=label,color=label,size=label))+theme_classic()+
      geom_freqpoly(binwidth=1)+
      facet_wrap(~Common.Name, scales = "free")+
      labs(x="Length (in)",y="Frequency")+
      scale_x_continuous(breaks = round(seq(min(pl$Fish.Length,na.rm = T),max(pl$Fish.Length,na.rm = T), by=1)))+
      theme(text = element_text(size = 17),legend.position = "bottom",legend.title = element_blank())+
      #scale_color_brewer(palette = "Dark2")+
      scale_color_manual(values = c("#999999","#009E73","#0072B2"))+
      #scale_fill_brewer(palette = "Dark2")+
      scale_size_manual(values = c(1,2,1))+guides(color=guide_legend(override.aes = list(size=1)))    
  })

  output$creBagTitle <- renderText(paste("Number of Fish Harvested Per Trip - ",curdb$Lake.Name[curdb$WBIC==lake()],sep = "")) #descriptive title for creel bag limit plot
  
  output$creBagLimits <- renderPlot({# plot comparing the distribution of bag limits of the selected species for the selected lake and its current and future lake class means
    
    validate(need(any(crCPE$WBIC==lake() & crCPE$Fish.Species.Code%in%codes()), message = paste("No creel harvest info available for the selected species in",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")))
    
    ptcL=crCPE%>%
      filter(crCPE$WBIC==lake() & crCPE$Fish.Species.Code%in%codes2())%>%
      select(Fish.Species.Code,Int.Party.Seq.No,Kept.Amt,Anglers.Amt)%>%
      inner_join(spk)
    ptcL=unique(ptcL[,c(1:5)])
    bLimit=regs%>%
      filter(wbic==lake() & regs$species%in%spk$regsNames[spk$Common.Name%in%input$vizPlots])%>%
      select(species,bag.limit)%>%
      left_join(spk,by=c("species"="regsNames"))%>%
      filter(!is.na(bag.limit))
    bLimit=unique(bLimit[,c(2,4)])
    ptcL=left_join(ptcL,bLimit)
    ptcL$Kept.Amt=as.numeric(ptcL$Kept.Amt)
    
    ggplot(data=ptcL)+theme_classic()+
      geom_point(aes(x=as.factor(Anglers.Amt),y=Kept.Amt),size=3)+
      facet_wrap(~Common.Name,scales = "free")+
      geom_hline(aes(yintercept = bag.limit),color="red")+
      labs(x="Number of Anglers",y="Number Harvested")+
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      theme(text = element_text(size = 17))
    
  })
  
  output$creelBoxWhisk <- renderPlot({# plot comparing the distribution of angler CPEs of the selected species for the selected lake and its current and future lake class means
    
    validate(need(any(crCPE$WBIC%in%curCreWBICS() & crCPE$WBIC%in%futCreWBICS() & crCPE$Fish.Species.Code%in%codes()), message = paste("No creel surveys available for the selected species in",curdb$Final.Lake.Class[curdb$WBIC==lake()],"and",futdb$Final.Lake.Class[futdb$WBIC==lake()],"lake classes",sep = " ")))
    
    pcL=crCPE%>%
      filter(crCPE$WBIC==lake() & crCPE$Fish.Species.Code%in%codes2())%>%
      mutate(effort=End.Time-Start.Time)%>%
      group_by(Int.Party.Seq.No,Fish.Species.Code)%>%
      summarise(CPE=mean((as.numeric(Caught.Amt)/effort), na.rm=T),
                cpeSD=sd((as.numeric(Caught.Amt)/effort), na.rm=T))
    
    pcC=crCPE%>%
      filter(crCPE$WBIC%in%curCreWBICS() & crCPE$Fish.Species.Code%in%codes2())%>%
      mutate(effort=End.Time-Start.Time)%>%
      group_by(Int.Party.Seq.No,Fish.Species.Code)%>%
      summarise(CPE=mean((as.numeric(Caught.Amt)/effort), na.rm=T),
                cpeSD=sd((as.numeric(Caught.Amt)/effort), na.rm=T))
    
    pcF=crCPE%>%
      filter(crCPE$WBIC%in%futCreWBICS() & crCPE$Fish.Species.Code%in%codes2())%>%
      mutate(effort=End.Time-Start.Time)%>%
      group_by(Int.Party.Seq.No,Fish.Species.Code)%>%
      summarise(CPE=mean((as.numeric(Caught.Amt)/effort), na.rm=T),
                cpeSD=sd((as.numeric(Caught.Amt)/effort), na.rm=T))
    
    pcL$label=rep(curdb$Lake.Name[curdb$WBIC==lake()],nrow(pcL))
    pcC$label=rep("Current Class",nrow(pcC))
    pcF$label=rep("Future Class",nrow(pcF))
    ord=c(unique(pcL$label),unique(pcC$label),unique(pcF$label))
    pc=rbind(pcL,pcC,pcF)
    pc=inner_join(pc,spk)
    pc$label=factor(pc$label,levels = ord)

    
    ggplot()+theme_classic()+
      geom_boxplot(data=pc,aes(x=label,y=CPE,fill=label))+
      facet_wrap(~Common.Name,scales = "free")+
      labs(x=element_blank(),y="CPUE")+
      theme(text = element_text(size=17),legend.title = element_blank(), axis.text.x = element_text(angle = 45,hjust=1))+
      scale_fill_manual(values = c("#999999","#009E73","#0072B2"))
    
  })
  

  ##### Population Metrics #####
  
  output$lakeCPEInfoTitle <- renderText({paste("Catch-per-unit-effort trends through time for",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")}) #title for plot of selected lake's CPUE trends
  output$lakeCPE <- renderPlot({ # trends in standardized electrofishing CPE for the selected lake and species
    
    validate(need(any(cpe$WBIC==lake()) & any(cpe$species%in%cpeNames()), message = paste("No CPE surveys available for the selected species in",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")))
    pd = cpe%>%
      filter(cpe$species%in%cpeNames() & cpe$WBIC==lake() & cpe$cpeMeasure%in%input$cpeMeas)%>%
      inner_join(spk, by=c("species"="standCPE"))
    
    ggplot(data = pd,aes(x=surveyYear,y=cpeVal,color=cpeMeasure))+theme_classic()+
      geom_point(size=2)+geom_line(size=2)+facet_wrap(~Common.Name,scales = "free")+
      labs(x="Year",y="CPE")+scale_color_discrete(name="CPE Measure")+theme(legend.position = "bottom", text = element_text(size = 17))+
      scale_color_brewer(palette = "Dark2")
    
  })
  output$curClassCPEInfoTitle <- renderText({paste("2020 lake class catch-per-unit-effort temporal trends:",curdb$Final.Lake.Class[curdb$WBIC==lake()], sep = " ")})#title for plot of selected lake's curent lake class CPUE trends
  output$curCPE <- renderPlot({# trends in standardized electrofishing CPE for the selected lake's current lake class and species
    
    pd = cpe%>%
      filter(species%in%cpeNames() & WBIC%in%curCpeWBICS() & cpeMeasure%in%input$cpeMeas)%>%
      group_by(surveyYear,species,cpeMeasure)%>%
      summarise(cpeVal=mean(cpeVal,na.rm=T))%>%
      inner_join(spk,by=c("species"="standCPE"))
    
    ggplot(data = pd,aes(x=surveyYear,y=cpeVal,color=cpeMeasure))+theme_classic()+
      geom_point(size=2)+geom_line(size=2)+facet_wrap(~Common.Name,scales = "free")+
      labs(x="Year",y="CPE")+scale_color_discrete(name="CPE Measure")+theme(legend.position = "bottom", text = element_text(size = 17))+
      scale_color_brewer(palette = "Dark2")
  })
  output$futClassCPEInfoTitle <- renderText({paste("2050 lake class catch-per-unit-effort temporal trends:",futdb$Final.Lake.Class[futdb$WBIC==lake()], sep = " ")})#title for plot of selected lake's future lake class CPUE trends
  output$futCPE <- renderPlot({# trends in standardized electrofishing CPE for the selected lake's future lake class and species
    pd = cpe%>%
      filter(species%in%cpeNames() & WBIC%in%futCpeWBICS() & cpeMeasure%in%input$cpeMeas)%>%
      group_by(surveyYear,species,cpeMeasure)%>%
      summarise(cpeVal=mean(cpeVal,na.rm=T))%>%
      inner_join(spk,by=c("species"="standCPE"))
    
    ggplot(data = pd,aes(x=surveyYear,y=cpeVal,color=cpeMeasure))+theme_classic()+
      geom_point(size=2)+geom_line(size=2)+facet_wrap(~Common.Name,scales = "free")+
      labs(x="Year",y="CPE")+scale_color_discrete(name="CPE Measure")+theme(legend.position = "bottom", text = element_text(size = 17))+
      scale_color_brewer(palette = "Dark2")
  })
  output$yoyBoxWhisk <- renderPlot({ # plot to compare the distribution of standardized YOY walleye electrofishing CPUEs for the selected lake and its current and future lake class
    
    yoy=yoyCPE[,c(1,6,8,9)]
    datL1=yoy%>%
      filter(yoy$MWBC==lake())%>%
      group_by(YEAR)%>%
      summarise(mean.A0CPE=mean(as.numeric(AGE0CPE),na.rm=T),
                sd.A0CPE=sd(as.numeric(AGE0CPE),na.rm = T),
                mean.A1CPE=mean(as.numeric(AGE1CPE),na.rm=T),
                sd.A1CPE=sd(as.numeric(AGE1CPE),na.rm=T))
    datL=data.frame(year=numeric(),meanCPE=numeric(),sdCPE=numeric(),ageClass=character(),label=character())
    if(nrow(datL1)>0){
      for(i in 1:nrow(datL1)){
        addDat=data.frame(year=rep(datL1$YEAR[i],2),meanCPE=c(datL1$mean.A0CPE[i],datL1$mean.A1CPE[i]),sdCPE=c(datL1$sd.A0CPE[i],datL1$sd.A1CPE[i]),ageClass=c("age0","age1"),label=rep(curdb$Lake.Name[curdb$WBIC==lake()],2))
        datL=rbind(datL,addDat)
      } 
      datL=datL[!is.na(datL$meanCPE),]
    }
    
    datC1=yoy%>%
      filter(yoy$MWBC%in%curdb$WBIC[curdb$Final.Lake.Class==lc1()])%>%
      group_by(YEAR)%>%
      summarise(mean.A0CPE=mean(as.numeric(AGE0CPE),na.rm=T),
                sd.A0CPE=sd(as.numeric(AGE0CPE),na.rm = T),
                mean.A1CPE=mean(as.numeric(AGE1CPE),na.rm=T),
                sd.A1CPE=sd(as.numeric(AGE1CPE),na.rm=T))
    datC=data.frame(year=numeric(),meanCPE=numeric(),sdCPE=numeric(),ageClass=character(),label=character())
    if(nrow(datC1)>0){
      for(i in 1:nrow(datC1)){
        addDat=data.frame(year=rep(datC1$YEAR[i],2),meanCPE=c(datC1$mean.A0CPE[i],datC1$mean.A1CPE[i]),sdCPE=c(datC1$sd.A0CPE[i],datC1$sd.A1CPE[i]),ageClass=c("age0","age1"),label=rep("Current",2))
        datC=rbind(datC,addDat)
      }  
      datC=datC[!is.na(datC$meanCPE),]
    }
    datF1=yoy%>%
      filter(yoy$MWBC%in%futdb$WBIC[futdb$Final.Lake.Class==lc2()])%>%
      group_by(YEAR)%>%
      summarise(mean.A0CPE=mean(as.numeric(AGE0CPE),na.rm=T),
                sd.A0CPE=sd(as.numeric(AGE0CPE),na.rm = T),
                mean.A1CPE=mean(as.numeric(AGE1CPE),na.rm=T),
                sd.A1CPE=sd(as.numeric(AGE1CPE),na.rm=T))
    datF=data.frame(year=numeric(),meanCPE=numeric(),sdCPE=numeric(),ageClass=character(),label=character())
    if(nrow(datF1)>0){for(i in 1:nrow(datF1)){
      addDat=data.frame(year=rep(datF1$YEAR[i],2),meanCPE=c(datF1$mean.A0CPE[i],datF1$mean.A1CPE[i]),sdCPE=c(datF1$sd.A0CPE[i],datF1$sd.A1CPE[i]),ageClass=c("age0","age1"),label=rep("Future",2))
      datF=rbind(datF,addDat)
    }  
      datF=datF[!is.na(datF$meanCPE),]
    }
    pdat=rbind(datL,datC,datF)
    pdat$label=factor(pdat$label,levels = c(curdb$Lake.Name[curdb$WBIC==lake()],"Current","Future"))
    pj=position_jitter(width = 0.2,seed = 1)
    
    ggplot(data=pdat)+theme_classic()+
      geom_boxplot(aes(x=label,y=meanCPE,fill=label))+
      labs(x="",y="CPE")+facet_wrap(~ageClass,scales = "free_y")+
      theme(text = element_text(size = 17),legend.position = 'bottom',legend.title = element_blank(),axis.text.x = element_text(angle=45,hjust=1))+
      scale_fill_manual(values = c("#999999","#009E73","#0072B2"))
  })
  output$yoyTemporal <- renderPlot({ # plot of trends through time for the YOY standardized electrofishing data for the selected lake and its current and future lake class
    yoy=yoyCPE[,c(1,6,8,9)]
    datL1=yoy%>%
      filter(yoy$MWBC==lake())%>%
      group_by(YEAR)%>%
      summarise(mean.A0CPE=mean(as.numeric(AGE0CPE),na.rm=T),
                sd.A0CPE=sd(as.numeric(AGE0CPE),na.rm = T),
                mean.A1CPE=mean(as.numeric(AGE1CPE),na.rm=T),
                sd.A1CPE=sd(as.numeric(AGE1CPE),na.rm=T))
    datL=data.frame(year=numeric(),meanCPE=numeric(),sdCPE=numeric(),ageClass=character(),label=character())
    if(nrow(datL1)>0){for(i in 1:nrow(datL1)){
      addDat=data.frame(year=rep(datL1$YEAR[i],2),meanCPE=c(datL1$mean.A0CPE[i],datL1$mean.A1CPE[i]),sdCPE=c(datL1$sd.A0CPE[i],datL1$sd.A1CPE[i]),ageClass=c("age0","age1"),label=rep(curdb$Lake.Name[curdb$WBIC==lake()],2))
      datL=rbind(datL,addDat)
    } 
      datL=datL[!is.na(datL$meanCPE),]}
    datC1=yoy%>%
      filter(yoy$MWBC%in%curdb$WBIC[curdb$Final.Lake.Class==lc1()])%>%
      group_by(YEAR)%>%
      summarise(mean.A0CPE=mean(as.numeric(AGE0CPE),na.rm=T),
                sd.A0CPE=sd(as.numeric(AGE0CPE),na.rm = T),
                mean.A1CPE=mean(as.numeric(AGE1CPE),na.rm=T),
                sd.A1CPE=sd(as.numeric(AGE1CPE),na.rm=T))
    datC=data.frame(year=numeric(),meanCPE=numeric(),sdCPE=numeric(),ageClass=character(),label=character())
    if(nrow(datC1)>0){for(i in 1:nrow(datC1)){
      addDat=data.frame(year=rep(datC1$YEAR[i],2),meanCPE=c(datC1$mean.A0CPE[i],datC1$mean.A1CPE[i]),sdCPE=c(datC1$sd.A0CPE[i],datC1$sd.A1CPE[i]),ageClass=c("age0","age1"),label=rep("Current",2))
      datC=rbind(datC,addDat)
    }  
      datC=datC[!is.na(datC$meanCPE),]}
    datF1=yoy%>%
      filter(yoy$MWBC%in%futdb$WBIC[futdb$Final.Lake.Class==lc2()])%>%
      group_by(YEAR)%>%
      summarise(mean.A0CPE=mean(as.numeric(AGE0CPE),na.rm=T),
                sd.A0CPE=sd(as.numeric(AGE0CPE),na.rm = T),
                mean.A1CPE=mean(as.numeric(AGE1CPE),na.rm=T),
                sd.A1CPE=sd(as.numeric(AGE1CPE),na.rm=T))
    datF=data.frame(year=numeric(),meanCPE=numeric(),sdCPE=numeric(),ageClass=character(),label=character())
    if(nrow(datF1)>0){for(i in 1:nrow(datF1)){
      addDat=data.frame(year=rep(datF1$YEAR[i],2),meanCPE=c(datF1$mean.A0CPE[i],datF1$mean.A1CPE[i]),sdCPE=c(datF1$sd.A0CPE[i],datF1$sd.A1CPE[i]),ageClass=c("age0","age1"),label=rep("Future",2))
      datF=rbind(datF,addDat)
    }  
      datF=datF[!is.na(datF$meanCPE),]}
    
    pdat=rbind(datL,datC,datF)
    pdat$label=factor(pdat$label,levels = c(curdb$Lake.Name[curdb$WBIC==lake()],"Current","Future"))
    pj=position_jitter(width = 0.2,seed = 1)
    ggplot(data = pdat)+theme_classic()+
      geom_pointrange(aes(x=year,y=meanCPE,ymax=(meanCPE+sdCPE),ymin=(meanCPE-sdCPE),color=label),size=1,position = pj)+
      geom_line(aes(x=year,y=meanCPE,color=label),size=1,position = pj)+
      facet_wrap(~ageClass, scales="free_y")+labs(x="Year",y="CPE")+
      theme(text=element_text(size = 17),legend.position = 'bottom',legend.title = element_blank())+coord_cartesian(ylim = c(0,NA))+
      scale_color_manual(values = c("#999999","#009E73","#0072B2"))
  })
  output$yoyLengthTable <- DT::renderDataTable({ # data table showing the length information for the YOYs caught during fall YOY sampling for the selected lake and mean values for the current and future lake class
    datL=yoyCPE[yoyCPE$MWBC==lake(),]
    datL$label=rep(curdb$Lake.Name[curdb$WBIC==lake()],nrow(datL))
    datC=yoyCPE[yoyCPE$MWBC%in%curdb$WBIC[curdb$Final.Lake.Class==lc1()],]
    datC$label=rep("Current",nrow(datC))
    datF=yoyCPE[yoyCPE$MWBC%in%futdb$WBIC[futdb$Final.Lake.Class==lc2()],]
    datF$label=rep("Future",nrow(datF))
    ddat=rbind(datL,datC,datF)
    display=ddat%>%
      group_by(label)%>%
      summarise(meanLenA0=round(mean(as.numeric(AGE0AVEL),na.rm=T),digits = 2),
                meanMinA0=round(mean(as.numeric(AGE0MINL),na.rm=T),digits = 2),
                meanMaxA0=round(mean(as.numeric(AGE0MAXL),na.rm=T),digits = 2),
                meanLenA1=round(mean(as.numeric(AGE1AVEL),na.rm=T),digits = 2),
                meanMinA1=round(mean(as.numeric(AGE1MINL),na.rm=T),digits = 2),
                meanMaxA1=round(mean(as.numeric(AGE1MAXL),na.rm=T),digits = 2))
    colnames(display)=c("","Average Age-0 Length","Average Minimum Age-0 Length","Average Maximum Age-0 Length","Average Age-1 Length","Average Minimum Age-1 Length","Average Maximum Age-1 Length")
    DT::datatable(display,rownames = F)
  })
  
  output$cpeDistText <- renderText({ # text to go with the standardized CPUE box and whisker plot
    paste("Standardized electrofishing survey catch-per-effort distributions for",curdb$Lake.Name[curdb$WBIC==lake()],"and its 2020 and 2050 lake class",sep=" ")
  })
  output$cpeBoxWhisk <- renderPlot({ # plot comparing the distributions of standardized CPE values for the selected species for the selected lake and its current and future lake class
    pL = cpe%>%
      filter(cpe$species%in%cpeNames() & cpe$WBIC==lake() & cpe$cpeMeasure%in%input$cpeMeas)%>%
      inner_join(spk, by=c("species"="standCPE"))%>%
      select(-WBIC)
    
    pC = cpe%>%
      filter(species%in%cpeNames() & WBIC%in%curCpeWBICS() & cpeMeasure%in%input$cpeMeas)%>%
      group_by(surveyYear,species,cpeMeasure)%>%
      summarise(cpeVal=mean(cpeVal,na.rm=T))%>%
      inner_join(spk,by=c("species"="standCPE"))
    
    pF = cpe%>%
      filter(species%in%cpeNames() & WBIC%in%futCpeWBICS() & cpeMeasure%in%input$cpeMeas)%>%
      group_by(surveyYear,species,cpeMeasure)%>%
      summarise(cpeVal=mean(cpeVal,na.rm=T))%>%
      inner_join(spk,by=c("species"="standCPE"))
    
    pC$label=rep("Current Class",nrow(pC))
    pF$label=rep("Future Class",nrow(pF))
    pL$label=rep(curdb$Lake.Name[curdb$WBIC==lake()],nrow(pL))
    
    pLCF=rbind(pL,pC,pF)
    
    ord=c(unique(pL$label),unique(pC$label),unique(pF$label))
    pLCF$label=factor(pLCF$label,levels = ord)
    
    ggplot()+theme_classic()+
      geom_boxplot(data=pLCF,aes(x=cpeMeasure,y=cpeVal,fill=label))+
      facet_wrap(~Common.Name)+
      theme(text=element_text(size=17),axis.text.x = element_text(angle = 45,hjust=1),legend.position = "bottom",legend.title = element_blank())+
      labs(x=element_blank(),y="CPE")+scale_fill_manual(values = c("#999999","#009E73","#0072B2"))
  })
  
  output$lakePETime <- renderPlot({ # plot showing the trends through time in the absolute abundance of adult walleye in the selected lake
    validate(need(nrow(pes[pes$MWBCODE==lake(),])>=1,message=paste("No population estimates available for",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")))
    datL=pes[pes$MWBCODE==lake() & !(pes$ModelINCode%in%c(10,11)),]
    datL$label=rep(curdb$Lake.Name[curdb$WBIC==lake()],nrow(datL))
    
    ggplot(datL)+theme_classic()+
      geom_pointrange(aes(x=MYEAR1,y=NUMBER,ymax=NUMHIGH,ymin=NUMLOW),size=1)+
      geom_line(aes(x=MYEAR1,y=NUMBER),size=1)+
      labs(x="Year",y="Adult Population Estimate")+
      theme(text = element_text(size = 17))
  })
  output$classCompDens <- renderPlot({ # plot showing the trends through time in the density of adult walleye per acre for the selected lake and its current and future lake class
    datC=pes%>%
      filter(pes$MWBCODE%in%curdb$WBIC[curdb$Final.Lake.Class==lc1()] & !(pes$ModelINCode%in%c(10,11)) & pes$MWBCODE!=lake() & !is.na(NUMBER) & !is.na(NUMLOW) & !is.na(NUMHIGH))%>%
      group_by(MYEAR1)%>%
      summarise(meanNacre=mean(NUMBER/REPAREA,na.rm=T),
                meanlowNacre=mean(NUMLOW/REPAREA,na.rm = T),
                meanhighNacre=mean(NUMHIGH/REPAREA,na.rm = T))%>%
      mutate(label="Current")
    
    datF=pes%>%
      filter(pes$MWBCODE%in%futdb$WBIC[futdb$Final.Lake.Class==lc2()] & !(pes$ModelINCode%in%c(10,11)) & pes$MWBCODE!=lake())%>%
      group_by(MYEAR1)%>%
      summarise(meanNacre=mean(NUMBER/REPAREA,na.rm=T),
                meanlowNacre=mean(NUMLOW/REPAREA,na.rm = T),
                meanhighNacre=mean(NUMHIGH/REPAREA,na.rm = T))%>%
      mutate(label="Future")
    
    datL=pes[pes$MWBCODE==lake() & !(pes$ModelINCode%in%c(10,11)),]
    datL$NUMACRE=datL$NUMBER/datL$REPAREA
    datL$numAcreLow=datL$NUMLOW/datL$REPAREA
    datL$numAcreHigh=datL$NUMHIGH/datL$REPAREA
    datL$label=rep(curdb$Lake.Name[curdb$WBIC==lake()],nrow(datL))
    datL=datL[,c(17,6,25:27)]
    colnames(datL)=colnames(datC)
    
    pdat=rbind(datL,datC,datF)
    pdat$label=factor(pdat$label, levels = c(curdb$Lake.Name[curdb$WBIC==lake()],"Current","Future"))
    
    #pj=position_jitter(width = 0.2,seed = 1)
    
    ggplot(pdat)+theme_classic()+
      geom_pointrange(aes(x=MYEAR1,y=meanNacre,ymax=meanhighNacre,ymin=meanlowNacre,color=label,size=label))+
      geom_line(aes(x=MYEAR1,y=meanNacre,color=label),size=1)+
      labs(x="Year",y="Adult Population Density (No./acre)")+
      theme(text = element_text(size = 17),legend.position = "bottom",legend.title = element_blank())+
      scale_size_manual(values = c(2,1.75,1))+guides(color=guide_legend(override.aes = list(size=1)))+
      scale_color_manual(values = c("#999999","#009E73","#0072B2"))
  })
  output$lakePEDetailText <- renderText({ # text to go with the lake specific pop. Est. plot
    paste("Detailed year-specific information on the adult populations estimates for", curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")
  })
  output$lakeDetailData <- DT::renderDataTable({ # data table containing information useful to managers on the data displayed in the plot of lake specific pop. ests.
    validate(need(nrow(pes[pes$MWBCODE==lake(),])>=1,message=paste("No population estimate details available for",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")))
    
    datL=pes[pes$MWBCODE==lake() & !(pes$ModelINCode%in%c(10,11)),]
    datL$NUMACRE=round(datL$NUMBER/datL$REPAREA,digits = 2)
    datL$numAcreLow=round(datL$NUMLOW/datL$REPAREA,digits=2)
    datL$numAcreHigh=round(datL$NUMHIGH/datL$REPAREA,digits = 2)
    datL$MWBCODE=as.numeric(datL$MWBCODE)
    display=datL%>%
      left_join(curdb[,1:2], by=c("MWBCODE"="WBIC"))
    display=display[,c(17,18,6,25,26,9,11,12,7,10,13,14,16,19:24)]
    colnames(display)=c("Year","WRC","Number.per.acre","Low.Number.acre","High.Number.acre","Number","Number.High","Number.Low","lbs.acre","lbs","High.lbs","Low.lbs","use.pop.est.code","Males","CV.Males","Females","CV.Females","TPE","CV.TPE")
    DT::datatable(display,rownames = F)
  })
  ##### Cost Information #####
  output$travelDesc.Text <- renderText({ # text to aid the user in understandnig the info on this tab
    "Click the bars (or space above them) to show WBIC, lake name, and NR probability for the lakes in a given lake class within the designated distance radius."
  })
  output$lakeCostInfo <- renderText({paste("Travel Cost information to",curdb$Lake.Name[curdb$WBIC==lake()],sep = " ")}) #generating label for cost information for the selected lake
  output$lakeCost <- renderTable({ # cost from the selected lake to each of the major cities
    tdat=td[td$WBIC==lake(),c(2,4)] # this gets you the distance in km to each of the major cities I selected
    
    tdat$cost=((((tdat$km*0.62)*0.621371)*.382273)+((((tdat$km*0.62)*0.621371)/45)*(((costs$MED_INC[costs$CityZip%in%cities2]*1.0897)/2000)*0.334)))*1.06 # multiply by 1.06 to get 2020 dollars from 2017 dollars
    colnames(tdat)=c("City","Distance to Lake (mi)","Cost (2020 dollars)")
    tdat
  })
  
  output$nearQualCost <- renderTable({ nearGood }) #static table on the cost to get to the nearest quality walleye fishery from each of the major cities regardless of lake class
  
  output$mi60Lakes <- renderPlot({ # number of alternative walleye options within a 60 mi drive
    
    singlake=hdro[hdro$WATERBOD_2==lake(),]
    center = st_centroid(singlake$geometry)
    close=hdro[st_is_within_distance(center,hdro,(1609*60))[[1]],]
    closeWLY=curdb[curdb$WBIC%in%close$WATERBOD_2,]
    closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
    actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
    actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
    ggplot()+theme_classic()+
      geom_bar(data = actualWLY,aes(x=Final.Lake.Class))+
      theme(axis.text.x = element_text(angle = 45,hjust = 1),text = element_text(size=17),plot.margin = unit(c(0,0,0,1),"cm"))+
      labs(x=element_blank(),y="Count",title = paste("Walleye Lakes (>0.5 NR prob) within 60 miles of",curdb$Lake.Name[curdb$WBIC==lake()],sep = " "))
  })
  output$mi60DT <- DT::renderDataTable({# table returning more information on the alternative options when the plot is clicked
    if(is.null(input$plot_clickCost60$x)){return()}else{
      singlake=hdro[hdro$WATERBOD_2==lake(),]
      center = st_centroid(singlake$geometry)
      close=hdro[st_is_within_distance(center,hdro,(1609*60))[[1]],]
      closeWLY=curdb[curdb$WBIC%in%close$WATERBOD_2,]
      closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
      actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
      actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
      actualWLY$Final.Lake.Class=as.factor(actualWLY$Final.Lake.Class)
      showRows=round(input$plot_clickCost60$x) == as.numeric(actualWLY$Final.Lake.Class)
      displayDF=actualWLY[showRows,c(1:3,18)];colnames(displayDF)=c("WBIC","Lake Name","County","NR probability")
      DT::datatable(displayDF,rownames = F)
    }
  })
  output$mi45Lakes <- renderPlot({# number of alternative walleye options within a 45 mi drive
    
    singlake=hdro[hdro$WATERBOD_2==lake(),]
    center = st_centroid(singlake$geometry)
    close=hdro[st_is_within_distance(center,hdro,(1609*45))[[1]],]
    closeWLY=curdb[curdb$WBIC%in%close$WATERBOD_2,]
    closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
    actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
    actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
    ggplot()+theme_classic()+
      geom_bar(data = actualWLY,aes(x=Final.Lake.Class))+
      theme(axis.text.x = element_text(angle = 45,hjust = 1),text = element_text(size=17),plot.margin = unit(c(0,0,0,1),"cm"))+
      labs(x=element_blank(),y="Count",title = paste("Walleye Lakes (>0.5 NR prob) within 45 miles",curdb$Lake.Name[curdb$WBIC==lake()],sep = " "))
  })
  output$mi45DT <- DT::renderDataTable({# table returning more information on the alternative options when the plot is clicked
    if(is.null(input$plot_clickCost45$x)){return()}else{
      singlake=hdro[hdro$WATERBOD_2==lake(),]
      center = st_centroid(singlake$geometry)
      close=hdro[st_is_within_distance(center,hdro,(1609*45))[[1]],]
      closeWLY=curdb[curdb$WBIC%in%close$WATERBOD_2,]
      closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
      actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
      actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
      actualWLY$Final.Lake.Class=as.factor(actualWLY$Final.Lake.Class)
      showRows=round(input$plot_clickCost45$x) == as.numeric(actualWLY$Final.Lake.Class)
      displayDF=actualWLY[showRows,c(1:3,18)];colnames(displayDF)=c("WBIC","Lake Name","County","NR probability")
      DT::datatable(displayDF,rownames = F)
    }
  })
  output$mi30Lakes <- renderPlot({# number of alternative walleye options within a 30 mi drive
    
    singlake=hdro[hdro$WATERBOD_2==lake(),]
    center = st_centroid(singlake$geometry)
    close=hdro[st_is_within_distance(center,hdro,(1609*30))[[1]],]
    closeWLY=curdb[curdb$WBIC%in%close$WATERBOD_2,]
    closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
    actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
    actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
    ggplot()+theme_classic()+
      geom_bar(data = actualWLY,aes(x=Final.Lake.Class))+
      theme(axis.text.x = element_text(angle = 45,hjust = 1),text = element_text(size=17),plot.margin = unit(c(0,0,0,1),"cm"))+
      labs(x=element_blank(),y="Count",title = paste("Walleye Lakes (>0.5 NR prob) within 30 miles",curdb$Lake.Name[curdb$WBIC==lake()],sep = " "))
  })
  output$mi30DT <- DT::renderDataTable({# table returning more information on the alternative options when the plot is clicked
    if(is.null(input$plot_clickCost30$x)){return()}else{
      singlake=hdro[hdro$WATERBOD_2==lake(),]
      center = st_centroid(singlake$geometry)
      close=hdro[st_is_within_distance(center,hdro,(1609*30))[[1]],]
      closeWLY=curdb[curdb$WBIC%in%close$WATERBOD_2,]
      closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
      actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
      actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
      actualWLY$Final.Lake.Class=as.factor(actualWLY$Final.Lake.Class)
      showRows=round(input$plot_clickCost30$x) == as.numeric(actualWLY$Final.Lake.Class)
      displayDF=actualWLY[showRows,c(1:3,18)];colnames(displayDF)=c("WBIC","Lake Name","County","NR probability")
      DT::datatable(displayDF,rownames = F)
    }
  })
  
  # future travel radius
  
  output$mi60LakesF <- renderPlot({# number of alternative walleye options within a 60 mi drive in 2050
    
    singlake=hdro[hdro$WATERBOD_2==lake(),]
    center = st_centroid(singlake$geometry)
    close=hdro[st_is_within_distance(center,hdro,(1609*60))[[1]],]
    closeWLY=futdb[futdb$WBIC%in%close$WATERBOD_2,]
    closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
    actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
    actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
    ggplot()+theme_classic()+
      geom_bar(data = actualWLY,aes(x=Final.Lake.Class))+
      theme(axis.text.x = element_text(angle = 45,hjust = 1),text = element_text(size=17),plot.margin = unit(c(0,0,0,1),"cm"))+
      labs(x=element_blank(),y="Count",title = paste("Walleye Lakes (>0.5 NR prob) within 60 miles of",futdb$Lake.Name[futdb$WBIC==lake()],sep = " "))
  })
  output$mi60DTF <- DT::renderDataTable({# table returning more information on the alternative options when the plot is clicked
    if(is.null(input$plot_clickCost60F$x)){return()}else{
      singlake=hdro[hdro$WATERBOD_2==lake(),]
      center = st_centroid(singlake$geometry)
      close=hdro[st_is_within_distance(center,hdro,(1609*60))[[1]],]
      closeWLY=futdb[futdb$WBIC%in%close$WATERBOD_2,]
      closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
      actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
      actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
      actualWLY$Final.Lake.Class=as.factor(actualWLY$Final.Lake.Class)
      showRows=round(input$plot_clickCost60F$x) == as.numeric(actualWLY$Final.Lake.Class)
      displayDF=actualWLY[showRows,c(1:3,15)];colnames(displayDF)=c("WBIC","Lake Name","County","NR probability")
      DT::datatable(displayDF,rownames = F)
    }
  })
  output$mi45LakesF <- renderPlot({# number of alternative walleye options within a 45 mi drive in 2050
    
    singlake=hdro[hdro$WATERBOD_2==lake(),]
    center = st_centroid(singlake$geometry)
    close=hdro[st_is_within_distance(center,hdro,(1609*45))[[1]],]
    closeWLY=futdb[futdb$WBIC%in%close$WATERBOD_2,]
    closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
    actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
    actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
    ggplot()+theme_classic()+
      geom_bar(data = actualWLY,aes(x=Final.Lake.Class))+
      theme(axis.text.x = element_text(angle = 45,hjust = 1),text = element_text(size=17),plot.margin = unit(c(0,0,0,1),"cm"))+
      labs(x=element_blank(),y="Count",title = paste("Walleye Lakes (>0.5 NR prob) within 45 miles",futdb$Lake.Name[futdb$WBIC==lake()],sep = " "))
  })
  output$mi45DTF <- DT::renderDataTable({# table returning more information on the alternative options when the plot is clicked
    if(is.null(input$plot_clickCost45F$x)){return()}else{
      singlake=hdro[hdro$WATERBOD_2==lake(),]
      center = st_centroid(singlake$geometry)
      close=hdro[st_is_within_distance(center,hdro,(1609*45))[[1]],]
      closeWLY=futdb[futdb$WBIC%in%close$WATERBOD_2,]
      closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
      actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
      actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
      actualWLY$Final.Lake.Class=as.factor(actualWLY$Final.Lake.Class)
      showRows=round(input$plot_clickCost45F$x) == as.numeric(actualWLY$Final.Lake.Class)
      displayDF=actualWLY[showRows,c(1:3,15)];colnames(displayDF)=c("WBIC","Lake Name","County","NR probability")
      DT::datatable(displayDF,rownames = F)
    }
  })
  output$mi30LakesF <- renderPlot({# number of alternative walleye options within a 30 mi drive in 2050
    
    singlake=hdro[hdro$WATERBOD_2==lake(),]
    center = st_centroid(singlake$geometry)
    close=hdro[st_is_within_distance(center,hdro,(1609*30))[[1]],]
    closeWLY=futdb[futdb$WBIC%in%close$WATERBOD_2,]
    closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
    actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] # using this criteria for now, but can be updated
    actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
    ggplot()+theme_classic()+
      geom_bar(data = actualWLY,aes(x=Final.Lake.Class))+
      theme(axis.text.x = element_text(angle = 45,hjust = 1),text = element_text(size=17),plot.margin = unit(c(0,0,0,1),"cm"))+
      labs(x=element_blank(),y="Count",title = paste("Walleye Lakes (>0.5 NR prob) within 30 miles",futdb$Lake.Name[futdb$WBIC==lake()],sep = " "))
  })
  output$mi30DTF <- DT::renderDataTable({# table returning more information on the alternative options when the plot is clicked
    if(is.null(input$plot_clickCost30F$x)){return()}else{
      singlake=hdro[hdro$WATERBOD_2==lake(),]
      center = st_centroid(singlake$geometry)
      close=hdro[st_is_within_distance(center,hdro,(1609*30))[[1]],]
      closeWLY=futdb[futdb$WBIC%in%close$WATERBOD_2,]
      closeWLY=closeWLY[!is.na(closeWLY$Lake.Name),]
      actualWLY=closeWLY[closeWLY$probability.recruit.1>=0.5,] #using this criteria for now, but can be updated
      actualWLY=actualWLY[!is.na(actualWLY$WBIC),]
      actualWLY$Final.Lake.Class=as.factor(actualWLY$Final.Lake.Class)
      showRows=round(input$plot_clickCost30F$x) == as.numeric(actualWLY$Final.Lake.Class)
      displayDF=actualWLY[showRows,c(1:3,15)];colnames(displayDF)=c("WBIC","Lake Name","County","NR probability")
      DT::datatable(displayDF,rownames = F)
    }
  })
  
  # travel cost based on user entered city
  
  output$curDistTabName <- renderText({paste("2020 Travel Distances and times from",input$cityIn,",",input$stateIn,"to the",input$nearN,"closest lakes for the chosen lake class(es).",sep = " ")}) # title for the section with the use selected lake name
  output$curTravelData <- DT::renderDataTable({ # table detailing travel distances and times in 2020 based on the user inputed city
    withProgress(message = "Calculating year 2020 travel distances",value = 0,{
      incProgress(0.1)
      curNear=closestCur(n=nButton(),city = cityButton(),stateAbrv = stateButton(),curClass = curdb)
      incProgress(0.1)
      curDrivingCosts=driveTime(city = cityButton(),stateAbrv = stateButton(),closestLakeOutput = curNear)
      incProgress(0.1)
      curDrivingCosts$distanceStraight_m=as.vector(curDrivingCosts$distanceStraight_m)
      incProgress(0.2)
      curDrivingCosts$disStraight_mi=round(as.vector(curDrivingCosts$distanceStraight_m)/1609,digits = 2)
      incProgress(0.2)
      curDrivingCosts$disDrive_mi=round(curDrivingCosts$distanceDrive_m/1609,digits = 2)
      incProgress(0.2)
      curDrivingCosts$timeDriving_hr=round(curDrivingCosts$timeDriving_s/3600,digits = 2)
      
      #subsetting to useful display columns
      display=curDrivingCosts[,c(4:7,11:14)]
      display=left_join(display,curdb[,c(1,3)],by=c("endWBIC"="WBIC"))
      colnames(display)=c("WBIC","Lake Name","Latitude","Longitude","Lake Class","Straight-line Distance (mi)","Driving Distance (mi)","Driving time (hr)","County")
      display=display[display$`Lake Class`%in%lClassButton(),c(1,2,9,5:8,3:4)]
      DT::datatable(display,rownames = F)
      incProgress(0.1)
    })

  })
  output$futDistTabName <- renderText({paste("2050 Travel Distances and times from",input$cityIn,",",input$stateIn,"to the",input$nearN,"closest lakes for the chosen lake class(es).",sep = " ")}) # title for the section with the user selected lake name
  output$futTravelData <- DT::renderDataTable({#table detailing travel distances and times in 2050 based on the user inputed city
    withProgress(message = "Calculating year 2050 travel distances", value = 0,{
      incProgress(0.1)
      futNear=closestFut(n=nButton(),city = cityButton(),stateAbrv = stateButton(),futClass = futdb)
      incProgress(0.1)
      futDrivingCosts=driveTime(city = cityButton(),stateAbrv = stateButton(),closestLakeOutput = futNear)
      incProgress(0.1)
      futDrivingCosts$distanceStraight_m=as.vector(futDrivingCosts$distanceStraight_m)
      incProgress(0.2)
      futDrivingCosts$disStraight_mi=round(as.vector(futDrivingCosts$distanceStraight_m)/1609,digits = 2)
      incProgress(0.2)
      futDrivingCosts$disDrive_mi=round(futDrivingCosts$distanceDrive_m/1609,digits = 2)
      incProgress(0.2)
      futDrivingCosts$timeDriving_hr=round(futDrivingCosts$timeDriving_s/3600,digits = 2)
      
      #subsetting to useful display columns
      display=futDrivingCosts[,c(4:7,11:14)]
      display=left_join(display,futdb[,c(1,3)],by=c("endWBIC"="WBIC"))
      colnames(display)=c("WBIC","Lake Name","Latitude","Longitude","Lake Class","Straight-line Distance (mi)","Driving Distance (mi)","Driving time (hr)","County")
      display=display[display$`Lake Class`%in%lClassButton(),c(1,2,9,5:8,3:4)]
      DT::datatable(display,rownames = F)
      incProgress(0.1)
    })
  })
  
  ##### Invasive Species #####
  
  output$invSppList <- renderText({ #listing the known invasives in the user selected lake
    validate(need(any(inv$WBIC==lake()), message = paste("No invasive species data available for ", curdb$Lake.Name[curdb$WBIC==lake()], sep = " ")))
    
    paste("The following invasive species are present in ",inv$WaterBodyName[inv$WBIC==lake()],": ",inv$invList[inv$WBIC==lake()],sep="")
  })
  output$meanInvText <- renderText({ #lake class mean numbber of invasives in the selected lake's lake class
    paste("Mean number of invasive species for 2020 lake class:",lc1(),"and 2050 lake class:",lc2(),"for",input$lname2,sep = " ")
  })
  output$meanInv <- renderTable(rownames = T,{ #frequency of occurrance for each of the invasives for the current and future lake classes of the selected lake
    cdat=inv[inv$WBIC%in%curdb$WBIC[curdb$Final.Lake.Class==lc1()],]
    fdat=inv[inv$WBIC%in%futdb$WBIC[futdb$Final.Lake.Class==lc2()],]
    display1=data.frame(nSystems=c(nrow(cdat),nrow(fdat)),Mean.Invasive.Count=c(round(mean(rowSums(cdat[,-c(1:3)]))),round(mean(rowSums(fdat[,-c(1:3)])))))
    colnames(display1)=c("Number of Systems","Mean Number of Invasives")
    classInvSum=t(display1);colnames(classInvSum)=c("Current (2020)","Future (2020)")
    
    classInvSum
  })
  output$invTableText <- renderText({# title for table describing the number of lakes in the current and future lake classes of the selected lake that have a given invasive species
    paste("Percentage of",curdb$Final.Lake.Class[curdb$WBIC==lake()],"systems that contain each invasive species for the 2020 and 2050 lake classes.",sep = " ")
  }) 
  output$ClassInv <- DT::renderDataTable({# table showing the proportion of lakes in the selected lake's current and future lake class that have each invasive species
    cdat=inv[inv$WBIC%in%curdb$WBIC[curdb$Final.Lake.Class==lc1()],]
    fdat=inv[inv$WBIC%in%futdb$WBIC[futdb$Final.Lake.Class==lc2()],]
    addCol=as.data.frame(matrix(NA,nrow=2,ncol = length(colnames(inv)[-c(1:3)])))
    colnames(addCol)=colnames(inv)[-c(1:3)]
    display2=cbind(nSystems=c(nrow(cdat),nrow(fdat)),addCol)
    sumsAddC=colSums(cdat[,-c(1:3)])
    sumsAddF=colSums(fdat[,-c(1:3)])
    display2[1,-1]=round((sumsAddC/display2$nSystems[1])*100)
    display2[2,-1]=round((sumsAddF/display2$nSystems[2])*100)
    
    #combining and displaying future and current data use a dynamic dt rander from DT package
    displaySpp=t(display2);colnames(displaySpp)=c("Current (2020)","Future (2050)")
    DT::datatable(displaySpp[-1,])
  })
  ##### Public Report Download #####
  output$summaryText <- renderUI({ #data summary for the selected lake
    quants=quantile(curdb$probability.recruit.1[curdb$Final.Lake.Class==curdb$Final.Lake.Class[curdb$WBIC==lake()]],probs=seq(0,1,by=0.05),na.rm = T)
    percentile=names(quants)[sum(quants<curdb$probability.recruit.1[curdb$WBIC==lake()])] #returns the percentile for the chosen lake compared to the NR for its lake class
    quantsF=quantile(futdb$probability.recruit.1[futdb$Final.Lake.Class==futdb$Final.Lake.Class[futdb$WBIC==lake()]],probs=seq(0,1,by=0.05),na.rm = T)
    percentileF=names(quants)[sum(quants<futdb$probability.recruit.1[futdb$WBIC==lake()])]
    
    str1=paste(curdb$Lake.Name[curdb$WBIC==lake()],"is a",curdb$Final.Lake.Class[curdb$WBIC==lake()],"system with a probability of natural walleye recruitment better than", percentile,"of other",curdb$Final.Lake.Class[curdb$WBIC==lake()],"systems.",sep = " ")
    str2=paste("Its predicted probability of natural recruitment is",curdb$probability.recruit.1[curdb$WBIC==lake()]," and its future lake class is",futdb$Final.Lake.Class[futdb$WBIC==lake()],"where the predicted probability of natural recruitment is better than",percentileF,"of all",futdb$Final.Lake.Class[futdb$WBIC==lake()],"systems in 2050",sep = " ")
    
    HTML(paste(str1,str2,sep = "  "))
  })
  output$downloadText <- renderText("Download a report to share with customers")
  output$report <- downloadHandler(# function to generate an approved report to share with the public through an Rmarkdown script. Downloads as a PDF that can be distributed to the public
    filename = "report.pdf",
    content = function(file){
      shiny::withProgress(message = "Compiling Report",
                          {
                            tempReport = file.path(tempdir(),"report_FME_submission.Rmd")
                            file.copy("report_FME_submission.Rmd",tempReport,overwrite = T)
                            
                            #setting up parameters to be read in and used in the report
                            params = list(curdb =curdb,
                                          futdb =futdb,
                                          yoyCPE =yoyCPE,
                                          pes =pes,
                                          lake =lake(),
                                          lakeName =curdb$Lake.Name[curdb$WBIC==lake()],
                                          lc1 =lc1(),
                                          lc2 =lc2()
                            )
                            rmarkdown::render(tempReport, output_file = file,
                                              params = params,
                                              envir = new.env(parent = globalenv())
                            )
                          })
      
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)


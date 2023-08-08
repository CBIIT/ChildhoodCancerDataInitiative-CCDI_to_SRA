#!/usr/bin/env Rscript

#Childhood Cancer Data Initiative - CCDI_to_SRA.R

#This script will take data from a validated CCDI submission manifest, and create an SRA submission file specifically for a CCDI project.

##################
#
# USAGE
#
##################

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CCDI_to_SRA.R --help


##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("dplyr","tidyr","readr","stringi","readxl","openxlsx","xlsx","janitor","optparse","jsonlite","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"))

#Load libraries.
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(tidyr,verbose = F))
suppressMessages(library(readr,verbose = F))
suppressMessages(library(xlsx,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(openxlsx,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(jsonlite,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(tools,verbose = F))
suppressMessages(library(stringi,verbose = F))

#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="A validated dataset file  based on the template CCDI_submission_metadata_template (.xlsx)", metavar="character"),
  make_option(c("-s", "--previous_submission"), type="character", default=NULL, 
              help="A previous SRA submission file (xlsx) from the same phs_id study.", metavar="character"),
  make_option(c("-t", "--template"), type="character", default=NULL, 
              help="A dbGaP SRA metadata template, 'phsXXXXXX'", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nCCDI_to_SRA v2.0.2")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file) | is.null(opt$template)){
  print_help(opt_parser)
  cat("Please supply the input file (-f) and/or template file (-t).\n\n")
  suppressMessages(stop(call.=FALSE))
}


#Data file pathway
file_path=file_path_as_absolute(opt$file)

template_path=file_path_as_absolute(opt$template)

if (!is.null(opt$previous_submission)){
  previous_submission_path=file_path_as_absolute(opt$previous_submission)
}

#A start message for the user that the manifest creation is underway.
cat("The SRA submission file is being made at this time.\n")


###########
#
# File name rework
#
###########

#Rework the file path to obtain a file name, this will be used for the output file.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])
ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))
path=paste(dirname(file_path),"/",sep = "")

output_file=paste(file_name,
                  "_SRA",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")


# Read in CCDI workbook, and then pull each node page out.
sheet_names=excel_sheets(path = file_path)
sheet_names=sheet_names[!sheet_names %in% c("README and INSTRUCTIONS","Dictionary","Terms and Value Sets")]

#Read in metadata page/file to check against the expected/required properties. 
workbook_list=list()

#create a list of all node pages with data
for (sheet in sheet_names){
  #read the sheet
  df=readWorkbook(xlsxFile = file_path,sheet = sheet)
  #create an emptier version that removes the type and makes everything a character
  df_empty_test=df%>%
    select(-type)%>%
    mutate(across(everything(), as.character))
  #remove empty rows and columns
  df_empty_test=remove_empty(df_empty_test,c("rows","cols"))
  
  #if there are at least one row in the resulting data frame, add it
  if (dim(df_empty_test)[1]>0){
    #if the only columns in the resulting data frame are only linking properties (node.node_id), do not add it.
    if (any(!grepl(pattern = "\\.",x = colnames(df_empty_test)))){
      #add the data frame to the workbook
      workbook_list=append(x = workbook_list,values = list(df_empty_test))
      names(workbook_list)[length(workbook_list)]<-sheet
    }
  }
}

#read in the template information
df_template=suppressMessages(read_xlsx(path = template_path, sheet = "Sequence_Data",col_names = TRUE))
df_template_terms=suppressMessages(read_xlsx(path = template_path, sheet = "Terms",col_names = FALSE))

#################
#
# Data frame setup
#
#################

df=workbook_list['sequencing_file'][[1]]

#If there are no sequencing data associated with the submission. Simply end the program. This will help with pipeline as it would normally error out at a point when there is no information.

if (is.null(df)){
  stop("No sequencing files were found in this submission, and the SRA generation will be skipped.")
}

if ('acl' %in% colnames(workbook_list['study_admin'][[1]])){
  df$acl=workbook_list['study_admin'][[1]]['acl'][[1]]
}else if ('acl' %in% colnames(workbook_list['study'][[1]])){
  df$acl=workbook_list['study'][[1]]['acl'][[1]]
}

df$study_name=workbook_list['study'][[1]]['study_name'][[1]]

#create data frame with the columns of the dbGaP template and the rows of the CCDI template
SRA_df=data.frame(matrix(ncol = dim(df_template)[2],nrow=dim(df)[1]))

#IF A BLANK TEMPLATE IS NOT USED, THIS PART WILL CREATE A SHIFTED OUTPUT FILE
colnames(df_template)[grep(pattern = "filename...18",x = colnames(df_template))]<-"filename...15_1"
colnames(df_template)[grep(pattern = "filetype...17",x = colnames(df_template))]<-"filetype...14_1"
colnames(df_template)[grep(pattern = "MD5_checksum...19",x = colnames(df_template))]<-"MD5_checksum...16_1"

colnames(SRA_df)<-colnames(df_template)

#FIX TO NON-BLANK TEMPLATE USE, DESTROY THE FILE COLUMNS PAST TWO INSTANCES
drop_name=grep(pattern = "filename...",x = colnames(SRA_df))
drop_type=grep(pattern = "filetype...",x = colnames(SRA_df))
drop_md5=grep(pattern = "MD5_checksum...",x = colnames(SRA_df))

if (any(length(drop_name)>2 | length(drop_type)>2 | length(drop_md5)>2)){
  if (length(drop_name)>2){
    drop_name=drop_name[3:length(drop_name)]
  }
  
  if (length(drop_type)>2){
    drop_type=drop_type[3:length(drop_type)]
  }
  
  if (length(drop_md5)>2){
    drop_md5=drop_md5[3:length(drop_md5)]
  }
  
  drop_all=c(drop_name,drop_type,drop_md5)
  
  SRA_df=SRA_df[,-drop_all]
}

SRA_df$phs_accession=stri_replace_all_fixed(str = stri_replace_all_fixed(str = df$acl,pattern = "['",replacement = ""), pattern = "']",replacement = "")

#A one to one addition of each row from the CCDI template to the dbGaP SRA template
SRA_df$sample_ID=df$sample.sample_id
SRA_df$library_ID=df$library_id
SRA_df$`title/short description`=df$study_name
SRA_df$`library_strategy (click for details)`=df$library_strategy
SRA_df$`library_source (click for details)`=df$library_source
SRA_df$`library_selection (click for details)`=df$library_selection
SRA_df$library_layout=df$library_layout
SRA_df$`platform (click for details)`=df$platform
SRA_df$instrument_model=df$instrument_model
SRA_df$design_description=df$design_description
SRA_df$`reference_genome_assembly (or accession)`=df$reference_genome_assembly
SRA_df$alignment_software=df$sequence_alignment_software
SRA_df$filetype...14=df$file_type 
SRA_df$filename...15=df$file_name
SRA_df$MD5_checksum...16=df$md5sum
SRA_df$Bases=df$number_of_bp
SRA_df$Reads=df$number_of_reads
SRA_df$coverage=df$coverage
SRA_df$AvgReadLength=df$avg_read_length
SRA_df$active_location_URL=stri_reverse(df$file_url_in_cds)
SRA_df=separate(SRA_df,active_location_URL,c("file","active_location_URL"),sep="/",extra="merge")%>%select(-file)
SRA_df$active_location_URL=paste(stri_reverse(SRA_df$active_location_URL),"/",sep = "")


#To preserve the data structure of the template, if a input file is missing a column it will look in SRA_df and if it returns a null, it instead returns a column of NAs.

for (col_name in colnames(df_template)){
  if (!col_name %in% colnames(SRA_df)){
    SRA_df[col_name]=NA
  }
}

reorder=colnames(SRA_df)[colnames(SRA_df) %in% colnames(df_template)]
reorder=colnames(df_template)[colnames(df_template) %in% reorder]


SRA_df=SRA_df%>%
  select(all_of(reorder),everything())


#Fix issue where design description has to be at least 250 characters long. To avoid creating new data that was not supplied, we instead will add spaces onto the end of the string until 250 characters are hit, and then add one period to prevent white space cleaning from removing our spaces.
for (row in 1:dim(SRA_df)[1]){
  if (!is.na(SRA_df$design_description[row])){
    slength=nchar(SRA_df$design_description[row])
    if(slength<250){
      addlength=250-slength
      new_value=paste(SRA_df$design_description[row],paste(rep(x = " ",addlength),collapse = ""),".",sep = "")
      SRA_df$design_description[row]=new_value
    }
  }else if (is.na(SRA_df$design_description[row])){
    filler_val=paste(paste(rep(x = " ", 250), collapse = ""),".",sep = " ")
    SRA_df$design_description[row]=filler_val
  }
}


#If there is a row that does not contain a file, based on whether a file name is present, remove that row.
SRA_df=SRA_df[!is.na(SRA_df$filename...15),]

SRA_df=unique(SRA_df)

#Create data frames from Terms page of dbGaP SRA template or other drop down values.
df_strategy=df_template_terms[2:35,1:2]
colnames(df_strategy)<-c("Strategy","Description")
df_source=df_template_terms[38:44,1:2]
colnames(df_source)<-c("Source","Description")
df_selection=df_template_terms[47:79,1:2]
colnames(df_selection)<-c("Selection","Description")
df_layout=data.frame("Layout"=c('paired','single'))
df_platform=df_template_terms[82:87,1]
colnames(df_platform)<-c("Platform")
df_model=df_template_terms[82:99,2:7]
colnames(df_model)<-c("ILLUMINA","_LS454","PACBIO_SMRT","ION_TORRENT","OXFORD_NANOPORE","BGISEQ")
df_type=data.frame("filetype"=c('bam',"fastq","cram","sff","reference_fasta","OxfordNanopore_native","PacBio_HDF5","csv","tab","bam_index","cram_index","vcf","bcf","vcf_index"))


###############
#
# Specific fixes (Before verification occurs)
#
###############

#For Archer Fusion library strategies, they are not recognized in the SRA, so they will be turned into "OTHER". 
SRA_df$`library_strategy (click for details)`[grep(pattern = "Archer_Fusion",x = SRA_df$`library_strategy (click for details)`)]<-"OTHER"

#For CCDI we have a few more changes that need to occur to values to make sure the correct information is brought over.
SRA_df$`platform (click for details)`[grep(pattern = "Illumina",x = SRA_df$`platform (click for details)`)]<-"ILLUMINA"
#Fix output from CCDI to SRA, single-end -> single, paired-end -> paired
SRA_df$library_layout[grep(pattern = "single-end", SRA_df$library_layout)]<-"single"
SRA_df$library_layout[grep(pattern = "paired-end", SRA_df$library_layout)]<-"paired"
SRA_df$library_layout[grep(pattern = "aired end", SRA_df$library_layout)]<-"paired"
SRA_df$library_layout[grep(pattern = "ingle end", SRA_df$library_layout)]<-"single"
SRA_df$`library_source (click for details)`[grep(pattern = "DNA",x = SRA_df$`library_source (click for details)`)]<-"GENOMIC"
SRA_df$`library_source (click for details)`[grep(pattern = "RNA",x = SRA_df$`library_source (click for details)`)]<-"TRANSCRIPTOMIC"

#Fix the CRAI and BAI files to be index files
#This has to be done before any rearrangements to the files occur.
SRA_df$filetype...14[grep(pattern = "BAI", SRA_df$filetype...14)]<-"bai"
SRA_df$filetype...14[grep(pattern = "CRAI", SRA_df$filetype...14)]<-"crai"
# SRA_df$filetype...14[grep(pattern = "bai", SRA_df$filetype...14)]<-"bam_index"
# SRA_df$filetype...14[grep(pattern = "crai", SRA_df$filetype...14)]<-"cram_index"
SRA_df$filetype...14[grep(pattern = "tbi", SRA_df$filetype...14)]<-"vcf_index"


######################
#
# Double verification against template
#
######################

sink(paste(path,output_file,".txt",sep = ""))
cat("The following file will note when there are unexpected values or situations that occur when converting to the SRA submission file.\n\n")

#Fix all caps/ mixed caps versions of file types
for (type_pos in 1:dim(SRA_df)[1]){
  if (tolower(SRA_df$filetype...14[type_pos])%in%df_type$filetype){
    SRA_df$filetype...14[type_pos]=tolower(SRA_df$filetype...14[type_pos])
  }
}

#create a vector with all incorrect enums for removal from SRA_df
all_incorrect_values=character(0)

#Check against library_strategy
if(!all(unique(SRA_df$`library_strategy (click for details)`)%in%df_strategy$Strategy)){
  incorrect_values=unique(SRA_df$`library_strategy (click for details)`)[!unique(SRA_df$`library_strategy (click for details)`)%in%df_strategy$Strategy]
  incorrect_values=incorrect_values[!is.na(incorrect_values)]
  all_incorrect_values=c(all_incorrect_values,incorrect_values)
  if (!identical(incorrect_values,character(0))){
    cat(paste("\nThe following value, ", incorrect_values,", is an incorrect value and needs to be corrected based on the Terms page for library_strategy.", sep = ""))
  }
}

#Check against library_source
if(!all(unique(SRA_df$`library_source (click for details)`)%in%df_source$Source)){
  incorrect_values=unique(SRA_df$`library_source (click for details)`)[!unique(SRA_df$`library_source (click for details)`)%in%df_source$Source]
  incorrect_values=incorrect_values[!is.na(incorrect_values)]
  all_incorrect_values=c(all_incorrect_values,incorrect_values)
  if (!identical(incorrect_values,character(0))){
    cat(paste("\nThe following value, ", incorrect_values,", is an incorrect value and needs to be corrected based on the Terms page for library_source.", sep = ""))
  } 
}

#Check against library_selection
if(!all(unique(SRA_df$`library_selection (click for details)`)%in%df_selection$Selection)){
  incorrect_values=unique(SRA_df$`library_selection (click for details)`)[!unique(SRA_df$`library_selection (click for details)`)%in%df_selection$Selection]
  incorrect_values=incorrect_values[!is.na(incorrect_values)]
  all_incorrect_values=c(all_incorrect_values,incorrect_values)
  if (!identical(incorrect_values,character(0))){
    cat(paste("\nThe following value, ", incorrect_values,", is an incorrect value and needs to be corrected based on the Terms page for library_selection.", sep = ""))
  } 
}

#Check against library_layout
if(!all(unique(SRA_df$library_layout)%in%df_layout$Layout)){
  incorrect_values=unique(SRA_df$library_layout)[!unique(SRA_df$library_layout)%in%df_layout$Layout]
  incorrect_values=incorrect_values[!is.na(incorrect_values)]
  all_incorrect_values=c(all_incorrect_values,incorrect_values)
  if (!identical(incorrect_values,character(0))){
    cat(paste("\nThe following value, ", incorrect_values,", is an incorrect value and needs to be corrected based on the Terms page for library_layout.", sep = ""))
  } 
}

#Check against library_platform and instrument_model
if(!all(unique(SRA_df$`platform (click for details)`)%in%df_platform$Platform)){
  incorrect_values=unique(SRA_df$`platform (click for details)`)[!unique(SRA_df$`platform (click for details)`)%in%df_platform$Platform]
  incorrect_values=incorrect_values[!is.na(incorrect_values)]
  all_incorrect_values=c(all_incorrect_values,incorrect_values)
  if (!identical(incorrect_values,character(0))){
    cat(paste("\nThe following value, ", incorrect_values,", is an incorrect value and needs to be corrected based on the Terms page for platform.", sep = ""))
  }
}else{
  for (platform in unique(SRA_df$`platform (click for details)`)){
    platform_pos=grep(pattern = platform, x = colnames(df_model))
    if(!all(unique(SRA_df$instrument_model)%in%df_model[,platform_pos][[1]])){
      incorrect_values=unique(SRA_df$instrument_model)[!unique(SRA_df$instrument_model)%in%df_model[,platform_pos][[1]]]
      incorrect_values=incorrect_values[!is.na(incorrect_values)]
      all_incorrect_values=c(all_incorrect_values,incorrect_values)
      if (!identical(incorrect_values,character(0))){
        cat(paste("\nThe following value, ", incorrect_values,", is an incorrect value and needs to be corrected based on the Terms page for instrument_model.", sep = ""))
      }
    }
  }
}

if(!all(unique(SRA_df$filetype...14)%in%df_type$filetype)){
  incorrect_values=unique(SRA_df$filetype...14)[!unique(SRA_df$filetype...14)%in%df_type$filetype]
  incorrect_values=incorrect_values[!is.na(incorrect_values)]
  all_incorrect_values=c(all_incorrect_values,incorrect_values)
  if (!identical(incorrect_values,character(0))){
    cat(paste("\nThe following value, ", incorrect_values,", is an incorrect value and needs to be corrected based on the Terms page for file_type.", sep = ""))
  } 
}


#Remove any row that contains an incorrect value
if (!identical(all_incorrect_values,character(0))){
  cat("\n\nFor any unexpected values, please review the input file and determine if these files should be submitted to dbGaP for the SRA.\n\tAt this time, these files have been removed from the dbGaP SRA submission file.\n")
  
  incorrect_rows=c()
  
  for (inct_val in all_incorrect_values){
    SRA_row=grep(pattern = inct_val, x = SRA_df$filetype...14)
    incorrect_rows=c(incorrect_rows,SRA_row)
  }
  SRA_df=SRA_df[-incorrect_rows,]
}


#Note which row is missing information
for (row in 1:dim(SRA_df)[1]){
  strategy=is.na(SRA_df$`library_strategy (click for details)`[row])
  source=is.na(SRA_df$`library_source (click for details)`[row])
  selection=is.na(SRA_df$`library_selection (click for details)`[row])
  layout=is.na(SRA_df$library_layout[row])
  platform=is.na(SRA_df$`platform (click for details)`[row])
  model=is.na(SRA_df$instrument_model[row])
  type14=is.na(SRA_df$filetype...14[row])
  testNA=c(strategy,source,selection,layout,platform,model,type14)
  if (any(testNA)){
    cat(paste("\nSubmission entry for library_id ",SRA_df$library_ID[row]," contains missing values for required properties. Please make corrections in the output file or in the input file and run again.",sep=""))
  }
}


###############
#
# Data frame manipulation
#
###############

#Create vector to gather rows to remove if index files are moved.
positions_to_remove=c()

#Move all files from the same library_id to one row.

extra_file_cols=max(count(group_by(SRA_df,library_ID))$n)

if (extra_file_cols>1){
  if (extra_file_cols>2){
    for (extra in 2:(extra_file_cols-1)){
      SRA_df[paste("filetype...14_",extra,sep="")]=NA
      SRA_df[paste("filename...15_",extra,sep="")]=NA
      SRA_df[paste("MD5_checksum...16_",extra,sep="")]=NA
    }
  }
  for (uL_ID in unique(SRA_df$library_ID)){
    pos=grep(pattern = uL_ID, x = SRA_df$library_ID)
    min_pos=min(pos)
    add_pos=setdiff(pos,min_pos)
    positions_to_remove=c(positions_to_remove,add_pos)
    if (length(pos)>1){
      for (set_pos in 1:length(add_pos)){
        SRA_df[min_pos,paste("filename...15_",set_pos,sep="")]=SRA_df$filename...15[(add_pos[set_pos])]
        SRA_df[min_pos,paste("filetype...14_",set_pos,sep="")]=SRA_df$filetype...14[(add_pos[set_pos])]
        SRA_df[min_pos,paste("MD5_checksum...16_",set_pos,sep="")]=SRA_df$MD5_checksum...16[(add_pos[set_pos])]
      }
    }
  }
}

#Remove rows that were moved to a different file row
if (!is.null(positions_to_remove)){
  SRA_df=SRA_df[-positions_to_remove,]
}


#Ensure that library_ids are unique. With the code above, this line should never occur.
library_id_count=count(group_by(SRA_df,library_ID))
for (row in 1:dim(library_id_count)[1]){
  if (library_id_count$n[row]>1){
    cat(paste("\n\nThe following library_id, ",library_id_count$library_ID[row],", has multiple entries, but should be unique. Please make corrections to the input file and run again.",sep = ""))
  }
}



####################
#
# Concatenate previous SRA submission.
#
####################

if ((!is.null(opt$previous_submission))){
  df_ps=suppressMessages(read_xlsx(path = previous_submission_path,sheet = "Sequence_Data", guess_max = 1000000, col_types = "text"))
  
  #rename the column headers in the read in data frame to match the current header versions in the SRA_df data frame. i.e. filename...18 --> filename...15_2
  extra_col_num=length(grep(pattern = "filetype...", x = colnames(df_ps)))
  
  for (extra in 1:extra_col_num){
    if (extra != 1){
      type_grep= grep(pattern = "filetype...", x = colnames(df_ps)) [extra]
      name_grep= grep(pattern = "filename...", x = colnames(df_ps)) [extra]
      md5_grep= grep(pattern = "MD5_checksum...", x = colnames(df_ps)) [extra]
      
      colnames(df_ps)[type_grep]<-paste("filetype...14_",extra-1,sep = "")
      colnames(df_ps)[name_grep]<-paste("filename...15_",extra-1,sep = "")
      colnames(df_ps)[md5_grep]<-paste("MD5_checksum...16_",extra-1,sep = "")
    }
  }
  
  SRA_df=suppressMessages(unique(bind_rows(df_ps,SRA_df)))    


  if (length(unique(SRA_df$library_ID))!=length(SRA_df$library_ID)){
    cat("\nERROR: The are non-unique library ids with the addition of the new submission to the previous submission.\nPlease resolve these issues in the output of this file.\n\n")
    library_ids=count(group_by(SRA_df,library_ID))
    cat(paste("\nPlease refer to the following library_id list:\n",paste(unique(filter(library_ids,n>1)$library_ID),collapse = "\n"),sep = ""))
  }
  
}


sink()

####################
#
# fix column headers for write out
#
####################

colnames(SRA_df)[grep(pattern = "filename",x = colnames(SRA_df))]<-"filename"
colnames(SRA_df)[grep(pattern = "MD5_checksum",x = colnames(SRA_df))]<-"MD5_checksum"
colnames(SRA_df)[grep(pattern = "filetype",x = colnames(SRA_df))]<-"filetype"


#####################
#
# Write out
#
#####################

#Output file name based on phs_id.
phs_id=unique(df$acl)[1]
phs_id=gsub(pattern = "\\[\\'",replacement = "",x = phs_id)
phs_id=gsub(pattern = "\\'\\]",replacement = "",x = phs_id)
output_file=paste(phs_id,
                  "_SRA_submission",
                  sep="")

#Create new output directory
new_dir=paste(phs_id,"_SRA_submission_",stri_replace_all_fixed(str = Sys.Date(), pattern = "-",replacement = ""),"/",sep = "")
dir.create(path = paste(path,new_dir,sep = ""), showWarnings = FALSE)

path=paste(path,new_dir,sep = "")

wb=openxlsx::loadWorkbook(file = template_path)

writeData(wb=wb, sheet="Sequence_Data", SRA_df)

openxlsx::saveWorkbook(wb = wb,file = paste(path,output_file,".xlsx",sep = ""), overwrite = T)

cat(paste("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")) 


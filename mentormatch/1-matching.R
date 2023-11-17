
library(data.table)
library(openxlsx)
library(stringi)

# Load the prepared data
setwd("C:/Users/dkube1/OneDrive - UHG/Innovation/Machine-Learning/Mentor-Matching-C2E/Q2")
load("prepared-data.rds")

# Exclude mentors with no practices
mentors = mentors[!is.na(MentorPractices)]

# Assume 1 for MentorMax if NA
mentors[is.na(MentorMax),MentorMax:=1]

# Execute the simulation rounds
mentor_ids = rep(mentors$EMPLID,mentors$MentorMax)
N_MATCHES = length(mentor_ids)
N_SIMS = 3000
EARLY_STOP_LIMIT = 1500
EARLY_STOP_COUNTER = 0
TOP_SIM_SCORE = 0
TOP_SIM_ROUND = 0
best_simulation = list()

print(paste("Starting the simulation with",N_SIMS,"rounds"))
system.time({
for(i in 1:N_SIMS){
  
  matches_i = data.table(MENTOR=mentor_ids,MENTEE="")
  SIM_SCORE = 0
  mentees_copy = copy(mentees)
  mentor_ids_copy = copy(mentor_ids)
  
  # Use the best simulation mentor list to probabilistically
  # select the next mentor list
  # Otherwise, use a uniform random sample
  if(i > 10 & runif(1) <= 0.75){
    
    mentor_emails_copy = best_simulation[[3]]$MENTOR
    mentor_emails_copy = mentor_emails_copy[sample(N_MATCHES, N_MATCHES, prob=N_MATCHES:1)]
    
  }else{
    
    mentor_emails_copy = mentor_ids_copy[sample(N_MATCHES, N_MATCHES)]
    
  }
  
  # For every mentor, evaluate the available mentees based
  # on their grade alignment and skill elections
  for(id in mentor_ids_copy){
    
    mentor_i = mentors[EMPLID==id]
    
    # Mentors must be 1+ grade levels above every mentee
    # and no more than 3 grade levels higher
    # The mentor cannot be the mentee
    # The mentor must be outside of the mentee's direct hierarchy
    # The mentor cannot be the direct supervisor of the mentee
    mentees_i = mentees_copy[GradeInt < mentor_i$GradeInt]
    mentees_i = mentees_i[GradeInt+3 >= mentor_i$GradeInt]
    mentees_i = mentees_i[SUPERVISOR_ID != mentor_i$EMPLID & EMPLID != mentor_i$EMPLID]
    
    # Move to the next mentor if there aren't any available mentees to consider
    if(nrow(mentees_i)==0) next
    
    # Exclude mentees who are in the mentor's immediate hierarchy
    if(mentor_i$SUPV_FLAG & mentor_i$EMPLID != "000191420"){
      
      mentees_hierarchy = hierarchy[EMPLID %in% mentees_i$EMPLID]
      mentees_hierarchy = mentees_hierarchy[SUPERVISOR_ID==mentor_i$EMPLID]
      mentees_i = mentees_i[!(EMPLID %in% mentees_hierarchy$EMPLID)]
      
    }
    
    # Move to the next mentor if there aren't any available mentees to consider
    if(nrow(mentees_i)==0) next
    
    # Score matches based on skill elections
    practices_i = practices[EMPLID==id,.(EMPLID=MenteeEMPLID,PracticeMatchesCount)]
    mentees_i = merge(mentees_i,practices_i,by="EMPLID")
    
    # Move onto the next mentor if there aren't any mentees
    # with at least one skill match
    if(nrow(mentees_i)==0) next
    
    # Add a score based on cosine similarity of skill passages
    # Arbitrarily add 1 to avoid zero scores due to 0 similarity with skills
    mentees_i[,SKILL_SIMILARITY:=0]
    for(m in mentees_i$EMPLID){
      SIMILARITY = 1
      tryCatch({SIMILARITY = SIMILARITY + simil.matrix[id,m]},error=function(e){})
      mentees_i$SKILL_SIMILARITY = SIMILARITY
    }
    
    # Calculated score for every possible mentee based on
    # CLL practice matches and skill similarity
    mentees_i[,SCORE:=SKILL_SIMILARITY*PracticeMatchesCount]
    
    # Isolate the mentee with the top score
    # randomly choose one person if there are multiple with the same top score
    # Remove that mentee from the running list so they cannot be
    # evaluated and selected again
    MAX_SCORE = max(mentees_i$SCORE)
    mentees_i = mentees_i[SCORE==MAX_SCORE]
    N_MENTEES = nrow(mentees_i)
    mentee_match = mentees_i[sample(N_MENTEES,N_MENTEES)][1]$EMPLID
    matches_i[MENTOR==id & MENTEE==""][1]$MENTEE = mentee_match
    mentees_copy = mentees_copy[EMPLID!=mentee_match]
    SIM_SCORE = SIM_SCORE + round(MAX_SCORE,2)
    
  }
  
  # Penalty for unmatched mentors
  # Reduced by 10% for every unmatched mentor
  UNMATCHED_MENTORS = nrow(matches_i[MENTEE==""])
  SIM_SCORE = SIM_SCORE - (SIM_SCORE*UNMATCHED_MENTORS*0.10)
  
  # Record the simulation results if it's the best overall simulation
  NEW_BEST_SCORE = SIM_SCORE > TOP_SIM_SCORE
  if(NEW_BEST_SCORE){
    
    TOP_SIM_SCORE = SIM_SCORE
    TOP_SIM_ROUND = i
    best_simulation = list(
      "Simulation Score"=SIM_SCORE,
      "Simulation Round"=TOP_SIM_ROUND,
      "Matches"=matches_i
    )
    
  }
  
  # Print the running simulation results
  # For every simulation there is a 1% chance to print the running results
  if(runif(1) <= 0.01 | i == N_SIMS | NEW_BEST_SCORE){
    print(paste("Finished simulation round",i))
    print(paste("The running top simulation score is",TOP_SIM_SCORE,"from round",TOP_SIM_ROUND))
  }
  
  # Stop the whole simulation early if the early stop limit is reached
  EARLY_STOP_COUNTER = ifelse(NEW_BEST_SCORE,0,EARLY_STOP_COUNTER+1)
  if(EARLY_STOP_COUNTER==EARLY_STOP_LIMIT){
    print(paste("Stopping early after",EARLY_STOP_LIMIT,"rounds without an improvement"))
    break
  }
  
}
})

# Prepare the final data table
best_sim_dt = best_simulation[[3]]
setnames(best_sim_dt,c("Mentor","Mentee"))
MentorMerge = dt[,.(EMPLID,MentorEmail=Email,MentorGrade=Grade,MentorPractices,MentorSkills)]
best_sim_dt = merge(best_sim_dt,MentorMerge,by.x="Mentor",by.y="EMPLID")
MenteeMerge = dt[,.(EMPLID,MenteeEmail=Email,MenteeGrade=Grade,MenteePractices,MenteeSkills)]
best_sim_dt = merge(best_sim_dt,MenteeMerge,by.x="Mentee",by.y="EMPLID")
best_sim_dt = merge(
  best_sim_dt,
  practices[,.(EMPLID,MenteeEMPLID,PracticeMatches,PracticeMatchesCount)],
  by.x=c("Mentor","Mentee"),
  by.y=c("EMPLID","MenteeEMPLID")
  )
cols = colnames(best_sim_dt)
setcolorder(best_sim_dt,cols[grepl("Mentor",cols)])

# Table with the unmatched mentors and mentees
mentors_leftovers = mentors[
  !(EMPLID %in% best_sim_dt$Mentor),
  .(Election="Mentor",EMPLID,Email,Name,Grade)
]
mentees_leftovers = mentees[
  !(EMPLID %in% best_sim_dt$Mentee),
  .(Election="Mentee",EMPLID,Email,Name,Grade)
]
leftovers = rbindlist(list(mentors_leftovers,mentees_leftovers))
setorder(leftovers,-Election,EMPLID)

# Write the final results to file
sim_id = stri_rand_strings(1,5)
current_time = format(Sys.time(),"%Y%m%d%H%M%S")
filename1 = paste0("sim-",sim_id,"-matches-",best_simulation[[1]],"-",i,"-",current_time,".xlsx")
write.xlsx(best_sim_dt,filename1,overwrite=T)
filename2 = paste0("sim-",sim_id,"-leftovers-",current_time,".xlsx")
write.xlsx(leftovers,filename2,overwrite=T)



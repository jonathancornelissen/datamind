# Goal: have as little functions as possible a teacher has to use to create and upload a chapter to datamind 
# Extra functionality will be added in a later phase, feel free to contribute or email suggestions at jonathan@datamind.org! 

# 1. Author chapter:
author_chapter = function(chapdir, ...){
  author(deckdir=chapdir,  scaffold = system.file('skeleton', package = 'datamind'), ...)  
}

# 2. Preview chapter:

# 3. Log in to DataMind:
datamind_login = function(email, password){
  url = paste0("http://api.datamind.org/users/details.json?email=",email,"&password=",password)
  message("Logging in...");
  if(url.exists(url)){
    getURL(url)
    content = getURLContent(url)
    auth_token = fromJSON(content)$authentication_token;
    .DATAMIND_ENV <<- new.env();
    .DATAMIND_ENV$auth_token <- auth_token;    
    .DATAMIND_ENV$email <- email;    
    message("Logged in succesfully to DataMind.org!")
  }else{stop("Wrong user name  or password for DataMind.org.")} 
}

# 4. Upload chapter:
upload_chapter = function( inputFile, ... ){ 
  payload = slidify(inputFile, return_page=TRUE,...);  # Get the payload  
  theJSON = render_chapter_json_for_datamind(payload); # Get the JSON
  upload_json(theJSON); # Upload everything
}

##### HELP FUNCTIONS ##### 
upload_json = function(theJSON){ 
  base_url = "http://staging.api.datamind.org/chapters/create_from_r.json";
  redirect_url = "http://test.datamind.org/#/edit_course/";
  auth_token = .DATAMIND_ENV$auth_token;
  if(is.null(auth_token)){ 
    stop("Please login to DataMind first, using the datamind_login function")
  }else{
  url = paste0(base_url,"?auth_token=", auth_token);
  x = POST(url=url, body = theJSON, add_headers("Content-Type" = "application/json"))   
  course_id = content(x)$id;
  message("Course succesfully uploaded to DataMind.org");
  redirect_url = paste0(redirect_url,course_id);
  browseURL(redirect_url)
  }
} 


render_chapter_json_for_datamind = function(payload){
  # Extract basic course and chapter info: 
  outputList = list(course = payload$courseTitle, 
                    email  = .DATAMIND_ENV$email,
                    chapter=list(
                      description=payload$description, 
                      number=0,
                      title=payload$chapterTitle
                    ) 
  ) 
  
  # Extract for each exercise the relevant information:
  slides = payload$slides;
  exerciseList = list();
  for(i in 1:length(slides)){
    slide = slides[[i]]
    exerciseList[[i]] = list(  title    = slide$title,
                               assignment    = slide$content, 
                               number        = slide$num,
                               instructions  = slide$instructions$content, 
                               hint          = slide$hint$content,
                               sample_code   = extract_code( slide$sample_code$content ),
                               solution      = extract_code( slide$solution$content ),
                               sct           = extract_code( slide$sct$content) )
  }
  
  # Join everything: 
  outputList$chapter$exercises = exerciseList;  
  
  # Make JSON:
  toJSON(outputList)
}

extract_code = function(html){
  r = regexpr("<code class=\"r\">(.*?)</code>",html);
  code = regmatches(html,r);
  code = gsub("<code class=\"r\">|</code>","",code)
  return(code)
}


# FIRST implementation of checks for exercises
# $checks stores them
# $checks$result indicates as boolean whether everything is okay
# $checks$complete indicates whether all sections are present, if not, reports what's missing
# $checks$sctcorrect indicates whether runnin pre_exercise_code+solution+sct returns the expected result (true)

# Check 1 exercise:
check_exercise = function(exercise){
  exercise = check_exercise_completeness(exercise);
  exercise = check_exercise_sctcorrect(exercise);
  
  # Everything correct?
  exercise$checks$result = all(exercise$checks$complete, exercise$checks$sctcorrect)
}

# Check whether all obligatory sections are there (and potentially extra stuff per section)
check_exercise_completeness = function(exercise){
  
  return(exercise)
}

# Check whether the pre_exercise_code SCT
check_exercise_sct = function(exercise){
  
  return(exercise)  
}



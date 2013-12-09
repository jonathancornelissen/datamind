# Goal: have as little functions as possible a teacher has to use to create and upload a chapter to datamind 
# Extra functionality will be added in a later phase. 
# Feel free to contribute or just email suggestions at jonathan@datamind.org! 

# 1. Author chapter:
author_course = function(chapdir, ...){
  message(paste0("Creating course directory ",chapdir));
  message("Finished creating course directory...");
  message("Switching to course directory...");
  message("Initializing Git Repo");
  suppressMessages(author(deckdir=chapdir,  scaffold = system.file('skeleton', package = 'datamind'), ...) );
  message(paste0("Opening first chapter..."));
}

# 2. Preview chapter:
preview_chapter = function(inputFile,outputFile,...){
  payload = suppressWarnings(slidify(inputFile,return_page=TRUE,...) );
  
  # Show the preview:
  if (missing(outputFile)){
    outputFile = gsub("*.[R]?md$", '.html', inputFile)
  }
  browseURL(outputFile)
}

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
    message("Logged in successfully to DataMind.org with R! Make sure to log in with your browser to DataMind.org as well using the same account.")
  }else{stop("Wrong user name  or password for DataMind.org.")} 
}

# 4. Upload chapter:
upload_chapter = function( inputFile, open=TRUE, ... ){ 
  # not efficient, needs refactoring
  payload = suppressWarnings(slidify(inputFile, return_page=TRUE,...));  # Get the payload  
  theJSON = render_chapter_json_for_datamind(payload); # Get the JSON
  upload_chapter_json(theJSON,open=open); # Upload everything
} 

# 5. OR upload the entire course!
upload_course = function(open=TRUE){ 
  chapters = list.files(pattern="*.Rmd") # list all chapters in the directory  
  if (length(chapters)==0){ stop("There seem to be no chapters (in '.Rmd' format) in the current directory") }

  if (length(chapters) > 1){ 
    for(i in 1:length(chapters)){ 
      upload_chapter_within_course(chapters[i]);
    } 
  } 
  
  if (!file.exists("course.yml")){ # Just upload the last chapter in case no specific course file
    upload_chapter_within_course(chapters[length(chapters)], open=TRUE); #Upload and open the last chapter
    message("### Succesfully upload the course ###")
  }
  
  if (file.exists("course.yml")){
    upload_chapter_within_course(chapters[length(chapters)], open=FALSE); # Upload the last chapter
    course = yaml.load_file("course.yml");
    the_course_json = toJSON(course);
    upload_course_json(the_course_json);
  }
  
} 

##### HELP FUNCTIONS ##### 
upload_chapter_json = function(theJSON, open=TRUE){ 
  if(!exists(".DATAMIND_ENV")){
    stop("Please login to DataMind first, using the datamind_login function");    
  } 
  base_url     = "http://api.datamind.org/chapters/create_from_r.json";
  redirect_url = "http://www.datamind.org/#/edit_course/";
  auth_token = .DATAMIND_ENV$auth_token;    
  url = paste0(base_url,"?auth_token=", auth_token);
  
  x = try(httr:::POST(url = url, body = theJSON, add_headers(`Content-Type` = "application/json")))
  if( class(x) != "response" ){
    stop("Something went wrong. We didn't get a valid response from the DataMind server. Try again or contact info@datamind.org in case you keep experiencing this problem.")
  } else { 
    if(is.list(content(x)) ){ 
      if("course" %in% names(content(x)) ){  
        course = content(x)$course
        course_id = course$id
        course_title = course$title
        message(paste0("Course (id:",course_id,"):\n",course_title,"\nwas successfully uploaded to DataMind.org!"))
        redirect_url = paste0(redirect_url, course_id)
        if (open) {
          browseURL(redirect_url)
        } 
      } 
      if ("message" %in% names(content(x))) {
        message(content(x)$message);
      }
      if( "error" %in% names(content(x)) ){
        message(paste0("Something went wrong:\n", content(x)$error ));
      } 
    } else {
      message(paste0("Something went wrong. Please check whether your course was correctly uploaded to DataMind.org."));
    } 
  } 
}

upload_course_json = function(theJSON, open=TRUE){ 
  if(!exists(".DATAMIND_ENV")){
    stop("Please login to DataMind first, using the datamind_login function");    
  } 
  base_url     = "http://api.datamind.org/courses/create_from_r.json";
  redirect_url = "http://www.datamind.org/#/edit_course/";
  auth_token = .DATAMIND_ENV$auth_token;    
  url = paste0(base_url,"?auth_token=", auth_token);
  
  x = httr:::POST(url=url, body = theJSON, add_headers("Content-Type" = "application/json"))   
  course_id = content(x)$id;
  message("Course succesfully uploaded to DataMind.org");
  redirect_url = paste0(redirect_url,course_id);
  if (open){ browseURL(redirect_url) }
}

render_chapter_json_for_datamind = function(payload){
  if(!exists(".DATAMIND_ENV")){
    stop("Please login to DataMind first, using the datamind_login function");    
  }

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
    exerciseList[[i]] = list(  title         = html2txt(slide$title),
                               assignment    = clean_up_html(slide$content), 
                               number        = slide$num,
                               instructions  = clean_up_html(slide$instructions$content), 
                               hint          = clean_up_html(slide$hint$content),
                               sample_code   = extract_code( slide$sample_code$content ),
                               solution      = extract_code( slide$solution$content ),
                               sct           = extract_code( slide$sct$content), 
                               pre_exercise_code = extract_code( slide$pre_exercise_code$content) )
  }
  
  # Join everything: 
  outputList$chapter$exercises = exerciseList; 
  
  # Make JSON: 
  toJSON(outputList) 
}

extract_code = function(html){
  if(!is.null(html)){
  if(nchar(html)!=0){
  r = regexpr("<code class=\"r\">(.*?)</code>",html);
  code = regmatches(html,r);
  code = gsub("<code class=\"r\">|</code>","",code)
  code = html2txt(code);

  # solve bug: when quotes are within quotes, we need different type of quotes! e.g. "c('f','t','w')"
  code = gsub("[\\]\"","'",as.character(code));
  
  return(code)
  }}
} 

# Convenience function to convert html codes:
html2txt <- function(str){
  str = paste0("<code>",str,"</code>");
  xpathApply(htmlParse(str, asText=TRUE),"//body//text()", xmlValue)[[1]];
}

# Remove paragraphs:
clean_up_html = function(html){
#   html = gsub("<p>|</p>","",html)
    return(html)
}

upload_chapter_within_course = function(chapter,open=FALSE){ 
  message(paste("Start uploading chapter: ",chapter),"...");
  message("...uploading...")    
  invisible( capture.output( suppressMessages(upload_chapter(chapter,open=open)) ) );
  message(paste("Successfully uploaded chapter: ",chapter),"!");
  message("###"); 
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
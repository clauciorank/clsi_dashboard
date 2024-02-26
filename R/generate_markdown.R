generate_markdown <- function(file){
  # delete tmp files if exists
  file.remove(list.files('tmp/', full.names = TRUE))
  # create temp file
  tmp <- tempfile(fileext = '.pdf', tmpdir = 'tmp')
  # render markdown
  rmarkdown::render(file, output_format = 'pdf_document', output_file = tmp, output_dir = 'tmp')
  
  return(tmp)
}
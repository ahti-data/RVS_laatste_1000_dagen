tryCatch({
  setwd('c:\\Users\\MarcoGriepAHTI\\Git Repos\\RVS_laatste_1000_dagen\\dashboard')
  source('app.R', echo=FALSE)
  cat('\n✓ SUCCESS: app.R loaded without errors\n\n')
}, error=function(e){
  cat('\n✗ ERROR: ', e$message, '\n\n')
  traceback()
})

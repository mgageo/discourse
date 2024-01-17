# <!-- coding: utf-8 -->
#
# quelques fonctions pour discourse
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
Drive <- substr( getwd(),1,2)
baseDir <- sprintf("%s/web", Drive)
cfgDir <- sprintf("%s/web/geo/DISCOURSE", Drive)
varDir <- sprintf("%s/web.var/DISCOURSE", Drive)
setwd(baseDir)
dir.create(cfgDir, showWarnings = FALSE, recursive = TRUE)
dir.create(varDir, showWarnings = FALSE, recursive = TRUE)
DEBUG <- FALSE
source("geo/scripts/mga.R")
source("geo/scripts/misc.R")
source("geo/scripts/discourse_phpbb2.R")
source("geo/scripts/discourse_misc.R")
#
if ( interactive() ) {
  DEBUG <- TRUE
  graphics.off()
} else {
}

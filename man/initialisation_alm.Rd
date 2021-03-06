% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ALM-initialisation.R
\docType{methods}
\name{initialisation_alm}
\alias{initialisation_alm}
\title{Fonction \code{initialisation_alm}.}
\usage{
initialisation_alm(alm, init_oblig = TRUE)
}
\arguments{
\item{alm}{est un objet de type \code{\link{ALM}}.}

\item{init_oblig}{est un \code{logical} indiquant s'il est necessaire d'initialiser le PTF obligataire. Par defaut, sa valeur est egale a \code{TRUE}.}
}
\description{
Cette fonction permet d'initialiser un objet de type \code{\link{ALM}} : chargement des donnees, aggregation, calcul des probas...
}
\seealso{
Construction d'un objet de type \code{\link{ALM}} : \code{\link{load_alm}}.

Premiere aggregation d'un objet de type \code{\link{ALM}} : \code{\link{aggregation_alm}}.

Seconde aggregation pour les passifs \code{\link{ALM}} : \code{\link{aggregation_passif_2}}.
}
\author{
Damien Tichit pour Sia Partners
}

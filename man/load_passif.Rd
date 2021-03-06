% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Passif-load.R
\docType{methods}
\name{load_passif}
\alias{load_passif}
\title{Fonction \code{load_passif}.}
\usage{
load_passif(address)
}
\arguments{
\item{address}{est un objet de type \code{character} indiquant le dossier dans lequel se situe l'ensemble des donnees necessaires
pour la construction de l'objet.}
}
\description{
Cette fonction permet de charger les donnees pour un objet de type \code{\link{Passif}}. Les donnees auront ete prealablement disposees dans
une architecture propre a \code{SiALM}.
}
\details{
La creation d'un objet \code{\link{Passif}} necessite la creation de deux autres objets : \code{\link{Passif}}, \code{\link{Provision}} et \code{\link{HypPassif}}.
}
\seealso{
Construction d'un objet de type \code{\link{PTFPassif}} : \code{\link{load_ptf_passif}}.

Construction d'un objet de type \code{\link{HypPassif}} : \code{\link{load_hyp_passif}}.

Construction d'un objet de type \code{\link{Provision}} : \code{\link{load_provision}}.
}
\author{
Damien Tichit pour Sia Partners
}

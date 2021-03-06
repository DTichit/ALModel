% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HypActif-class.R
\docType{class}
\name{HypActif}
\alias{HypActif}
\title{Classe \code{HypActif}}
\description{
Cette classe aggrege l'ensemble des hypotheses relatives au passif de la compagnie d'assurance : portefeuille cible, frais financiers.
}
\section{Slots}{

\describe{
\item{\code{ptf_cible}}{est un objet de la classe \code{\link{PTFCible}} representant le portfeuille financier cible.}

\item{\code{esg_simu}}{est une \code{list} reprenant les donnees de l'ESG pour une simulation.}

\item{\code{frais_fin}}{est un objet de la classe \code{\link{data.frame}} contenant les hyptheses relatives aux frais financiers.}

\item{\code{revalo_treso}}{est un \code{numeric} indiquant le taux auquel est revalorise la tresorerie.}
}}

\seealso{
Le parametre \code{esg_simu} se met a jour grace a la fonction \code{\link{update_esg}}
}
\author{
Damien Tichit pour Sia Partners
}
\keyword{classes}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Immobilier-rebalancement.R
\docType{methods}
\name{rebalancement_immobilier}
\alias{rebalancement_immobilier}
\title{Fonction \code{rebalancement_immobilier}}
\usage{
rebalancement_immobilier(immo, alloc_cible)
}
\arguments{
\item{immo}{est un objet de type \code{\link{Immobilier}}. Ce parametre represente le ptf actuel de la compagnie.}

\item{alloc_cible}{est un \code{numeric}. Ce parametre indique l'allocation cible a atteindre.}

\item{immo_cible}{est un objet de type \code{\link{Immobilier}}. Ce parametre represente le ptf cible.}
}
\description{
Cette fonction permet de rebalancer le portfeuille immobilier vers un portfeuile cible.
Le montant total, en valeur de marche, du portefeuille cible est renseigne dans le parametre \code{alloc_cible}.
}
\author{
Damien Tichit pour Sia Partners
}

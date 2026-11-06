#' Test du Chi-deux d'Ajustement (Goodness-of-Fit)
#'
#' @description
#' Compare une distribution observée à une distribution théorique (attendue).
#'
#' @param observed Un vecteur numérique des fréquences observées.
#' @param expected Un vecteur numérique des fréquences théoriques attendues (doit avoir la même somme que `observed`).
#'
#' @return Une liste de classe `htest` contenant la statistique du Chi-deux, les degrés de liberté et la p-valeur.
#' @export
#'
#' @examples
#' obs <- c(50, 30, 20)
#' exp <- c(40, 40, 20)
#' chi2_ajustement(obs, exp)
chi2_ajustement <- function(observed, expected) {
  if (sum(observed) != sum(expected)) {
    warning("La somme des fréquences observées est différente de la somme des attendues.")
  }

  res <- stats::chisq.test(x = observed, p = expected / sum(expected), rescale.p = TRUE)
  return(res)
}


#' Test du Chi-deux d'Indépendance
#'
#' @description
#' Teste l'indépendance de deux variables catégorielles à partir d'un tableau de contingence.
#'
#' @param data Un tableau de contingence (matrice ou data.frame).
#'
#' @return Un objet de type `htest` avec les résultats du test d'indépendance.
#' @export
#'
#' @examples
#' tab <- matrix(c(30, 10, 20, 40), nrow = 2,
#'               dimnames = list(Ligne = c("A", "B"), Colonne = c("X", "Y")))
#' chi2_independance(tab)
chi2_independance <- function(data) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("L'argument 'data' doit être une matrice ou un data.frame.")
  }

  res <- stats::chisq.test(data)
  return(res)
}


#' Simulation de la Distribution du Chi-deux
#'
#' @description
#' Génère des valeurs aléatoires suivant une loi du Chi-deux pour visualiser
#' l'évolution de la distribution selon les degrés de liberté.
#'
#' @param df Degrés de liberté (entier positif).
#' @param n Nombre de simulations (par défaut 1000).
#'
#' @return Un vecteur numérique contenant les valeurs simulées.
#' @export
#'
#' @examples
#' valeurs <- plot_chi2_simulation(df = 4, n = 500)
#' hist(valeurs, main = "Simulation de la loi du Chi-2 (df = 4)", col = "steelblue", border = "white")
plot_chi2_simulation <- function(df = 3, n = 1000) {
  if (df <= 0) {
    stop("Le degré de liberté 'df' doit être strictement positif.")
  }

  simulations <- stats::rchisq(n = n, df = df)
  return(simulations)
}

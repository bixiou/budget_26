# Guide étape par étape — Enquête Budget 26

## Contexte

Je suis Adrien Fabre, chercheur en économie au CNRS. J'ai mené une enquête sur les préférences budgétaires des Français. Les données et figures sont sur le dépôt github.com/bixiou/budget_26. Ce guide te demande de rédiger une note académique (`.tex`) et une tribune (`.docx`) en mon nom, en te focalisant sur deux questions : **`budget`** et **`effect_program`**. Ignore complètement les autres questions de l'enquête.

### Fichiers clés à lire en priorité

| Fichier | Usage |
|--------|-------|
| `data_ext/codebook.csv` | Formulation exacte des questions |
| `xlsx/budget.xlsx` | Résultats descriptifs de la question `budget` |
| `xlsx/effect_program.xlsx` | Résultats descriptifs de la question `effect_program` |
| `tables/budget_policy_table.csv` | Table agrégée (colonne `amount` = économie en milliards €) |
| `figures/budget.pdf` | Figure principale `budget` |
| `figures/effect_program.pdf` | Figure principale `effect_program` |
| `tables/FR_bold.tex` | Représentativité de l'échantillon |
| `data_ext/budget_policies.xlsx` | Positionnement politique des mesures (colonne `leaning`) |
| `data_ext/vote_progressiste.xlsx` | Résultats de la conférence de consensus Les Progressistes |

---

## Étape 1 — Prise de connaissance des données

- [x] Lire `data_ext/codebook.csv` pour comprendre la formulation de toutes les questions
- [x] Lire `xlsx/budget.xlsx` et `tables/budget_policy_table.csv` pour comprendre les résultats de la question `budget` (quelles mesures sont soutenues, à quelle hauteur, quel effet budgétaire)
- [x] Lire `xlsx/effect_program.xlsx` pour comprendre les résultats de la question `effect_program`
- [x] Lire `tables/FR_bold.tex` pour noter les statistiques de représentativité de l'échantillon
- [x] Lire `data_ext/budget_policies.xlsx` pour comprendre le positionnement politique (`leaning`) de chaque mesure : -1 = pénalise les riches ; 0 = coûte à tous ; 0.5 = sectoriel ; 1 = réduit l'État social ; 2 = pénalise les étrangers
- [x] Consulter sur internet les trois initiatives mentionnées dans l'introduction :
  - Les notes et l'outil interactif du CAE sur le budget
  - La conférence de consensus de la plateforme Les Progressistes (voir aussi `data_ext/vote_progressiste.xlsx`)
  - budget-citoyen.fr

---

## Étape 2 — Analyses complémentaires (`code_budget/3_analyse.R`)

> Effectue ces analyses si tu as les droits d'exécution R ; sinon passe à l'étape 3.

- [x] **Déterminants socio-démographiques** : régressions du soutien aux mesures `budget` et `effect_program` en fonction du vote, du revenu, de l'âge, du genre, du diplôme
- [x] **Corrélations entre mesures** : matrice de corrélation du soutien aux différentes mesures `budget` ; identifier des groupes de mesures positivement corrélées
- [x] **Corrélations entre répondants** : clustering des répondants selon leurs préférences ; comparer le profil de vote de chaque cluster
- [x] **Paquets majoritaires** : identifier des paquets de mesures pour lesquels une majorité conjointe existe (i.e. une majorité soutient *chacune* des mesures du paquet) ; noter le profil de vote de cette majorité
- [x] Documenter les résultats de ces analyses sous forme de commentaires dans `3_analyse.R`

---

## Étape 3 — Note académique (`papers/budget.tex`)

- [x] Créer le fichier `papers/budget.tex` avec la structure suivante :

### 3a. Abstract
- [x] Résumer en 150 mots : objectif, méthode, principaux résultats sur `budget` et `effect_program`

### 3b. Introduction
- [x] Décrire le contexte budgétaire français actuel (déficit, objectifs de réduction)
- [x] Mentionner les trois initiatives : outil CAE, conférence Les Progressistes, budget-citoyen.fr
- [x] Annoncer la structure de la note

### 3c. Méthode
- [x] Décrire le questionnaire (design, nombre de questions, modalités de réponse)
- [x] Décrire l'échantillon : taille, recrutement (Bilendi/Qualtrics), pondération
- [x] Inclure le tableau de représentativité issu de `tables/FR_bold.tex`

### 3d. Résultats
- [x] Présenter les résultats de la question `budget` : quelles mesures sont soutenues/rejetées, statistiques descriptives, lien avec l'effet budgétaire (`amount`)
- [x] Inclure `figures/budget.pdf`
- [x] Présenter les résultats de la question `effect_program`
- [x] Inclure `figures/effect_program.pdf`
- [x] Si l'étape 2 a été effectuée : présenter les résultats des analyses complémentaires (déterminants, clusters, paquets)

### 3e. Conclusion
- [x] Synthétiser les enseignements principaux
- [x] Mentionner les limites de l'étude

---

## Étape 4 — Tribune (`papers/budget.docx`)

> Tribune destinée au *Monde*, **maximum 6 000 signes espaces compris**.

- [x] Créer `papers/budget.docx`
- [x] **Accroche** : partir d'un fait saillant ou d'un paradoxe issu des résultats
- [x] **Résultats descriptifs** : exposer les statistiques clés de `budget` et `effect_program` de façon accessible
- [x] **Analyse politique** : en s'appuyant sur `leaning` dans `data_ext/budget_policies.xlsx` et sur les paquets majoritaires identifiés à l'étape 2, analyser :
  - Quelles forces politiques (gauche, droite, extrême-droite) peuvent porter un programme permettant de réduire le déficit
  - Quelles coalitions de citoyens sont majoritaires pour un programme donné qui réduit le déficit
- [x] **Constitutionnalité** : mentionner brièvement les doutes sur la constitutionnalité des mesures d'extrême-droite (`leaning = 2`)
- [x] Vérifier que le texte ne dépasse pas 6 000 signes

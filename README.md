# BABELIS
Imaginez un professionnel de la santé, récemment diplômé ou à la recherche de nouvelles opportunités, cherchant à exercer sa profession en France. L'une de ses principales préoccupations est de savoir où se trouvent les besoins les plus importants en matière de services de santé. C'est une réalité que de nombreuses régions en France souffrent d'une pénurie de spécialistes médicaux, ce qui crée des opportunités précieuses pour les professionnels de la santé de s'installer là où leur expertise est le plus nécessaire. C'est là que notre application Shiny entre en jeu.
 
Elle a été conçue pour aider les professionnels de la santé à prendre des décisions éclairées sur la décision de leur lieu d'exercice en France. Notre application fournit des informations essentielles sur la répartition des professionnels de  santé et de la population dans les différentes régions du pays et notamment où il y a potentiellement moins de professionnels par rapport à la population.


 ## Description des données : 
Pour ceci, nous avions à notre disposition un large jeu de données :

(source : https://data.opendatasoft.com/explore/?disjunctive.language&disjunctive.source_domain_title&disjunctive.theme&disjunctive.semantic.classes&disjunctive.semantic.properties&sort=explore.popularity_score&q=vin&refine.source_domain_title=Data+ameli&fbclid=IwAR0ehSCcWvpoUJjjbChbdE1sV4f7Qho0U7VcEr2shQf23YYTyQy9EmBipuo) 

Cette base de données un peu plus de 2 million d’observations, fournissant l’effectif, à la maille année (2010<année<2021, profession santé, région, département, classe d’âge et sexe.
Afin de ramener cet effectif à la population, obtenant le nombre d'habitant par observation (cf maille décrite ci-dessus) nous avons fusionné notre jeu de données avec un autres jeux de données : 
- un donnant l’effectif total de la pop par région, département et par année (allant de 2014 à 2021) 

Nous avons aussi merge avec : 

 - une base de données AMELI détaillant les honoraires sans dépassement moyens (n’a pas été un jeu de données essentiel à la construction de l’application mais apporte des infos en plus pour les utilisateurs)
 - une base de données avec la patientèle_unique par année, profession santé, region, département.

Organisation en dashboard (tableau de bord interactif ). Nous avons fait ce choix car visuellement nous trouvions que c’était plus clair pour l’utilisateur.
Exploration :
  Graphiques
  Carte 
  Data
Infos (résume d’une autre manière la requête de l’utilisateur)

## Perspectives :

Rajouter les lieux précis des cabinets des praticiens (notammenet pour les profession comme les kinésithérapeutes) sur la carte

Prendre en compte l’âge des populations par région pour adapter les besoins (ex: si pop jeune alors pédiatres, si vieux alors plus besoin de cardiologues)

Enrichir notre base de données actuelle, qui s'arrête à l'année 2021, en intégrant régulièrement les données du site Ameli. Cette mise à jour continue permettra à notre application de rester à jour et de fournir aux utilisateurs les informations les plus récentes.
rajouter la possibilité de voir diff entre hommes et femmes





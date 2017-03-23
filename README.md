# ModeleDynamiquePop
Suivi du développement du modèle dynamique de population du saumon de l'Allier.

Dans la branche "Master" on retrouve tous les éléments relatifs à la dernière version validée du modèle avec
les différents répertoires du projet :

- script_openbugs = tous les fichiers nécessaires pour faire tourner le modèle sous Openbugs.
  - temp model.odc = le code du modèle
  - data.odc = les données nécessaires pour faire tourner le modèle
  - init.odc = les inits
  - script.odc = le script listant tous les SampleSet, indiquant le chemin d'accès au fichier temp model, data et init
                  ainsi que le choix sur le nombre d'itération à lancer et le thin
  - script_coda.odc = nécessaire pour extraire les CODA une fois que le modèle a tourné
  - script_coda_parameters.odc = idem 
  - script_coda_simulation.odc = idem
- script_R = tous les scripts R ou Rnw permettant de réaliser les sorties ou simulations sur le modèle
- figures = toutes les figures relatives au modèle présent dans la branche Master
- rapports = les rapports réalisés sur le projet

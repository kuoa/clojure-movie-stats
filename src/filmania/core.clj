(ns filmania.core
  (require [clojure.data.csv :as csv]
           [clojure.java.io :as io]))


(defn csv-seq
  "Retourne une séquence à partir d'un fichier CSV."
  [filename]
  (with-open [in-file (io/reader filename)]
    (doall
     (csv/read-csv in-file))))



(defn parse-movie
  "Construit un enregistrement de film depuis un entrée lue depuis CSV."
  [title-year genres]
  (let [r     (re-seq #"(.+) \((\d\d\d\d)\)$" title-year)
        title (get (first r) 1)]
    (try
      (let [year (Integer/parseInt (get (first r) 2))]
        {:title  title
         :year   year
         :genres (set (filter #(not= % "(no genres listed)") (clojure.string/split genres #"\|")))})
      (catch Exception _ nil))))

(defn movie-map
  "Construit une map de films à partir d'un base en CSV."
  [csv]
  (reduce (fn [m [movie-id title-year genres]]
            (if-let [movie (parse-movie title-year genres)]
              (assoc m (Integer/parseInt movie-id) movie)
              m))
          {} csv))

;; Attention: gros fichier
(def movie-filename "resources/ml-latest-small/movies.csv")

(def movies (movie-map (rest (csv-seq movie-filename))))

(def small-movies (take 30 movies))
(def smaller-movies (take 5 movies))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             FIST PART
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Archive size :    (archive-size movies)                     => 8918
;; Science-fiction : (count (films-by-genre "Sci-Fi" movies))  => 743
;; Romance :         (count (films-by-genre "Romance" movies)) => 1564


(defn archive-size
  [archive]
  (count archive))

(defn get-genres-set
  [movie]
  (:genres (get movie 1)))

(defn films-by-genre
  "Returns the number of movies of a specific genre in the archive."
  [genre archive]
  (filter (fn [movie]
            (some #(= genre %) (get-genres-set movie))) archive))


(defn all-genres
  "Returns a set of all the genres present in the archive"
  [archive]
  (reduce (fn [result movie]
            (into result
                  (get-genres-set movie))) #{} archive))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Max Genre :   (max-genre (card-genres movies))    ["Drama" 4310]
;; Min Genre :   (min-genre (card-genres movies))    ["Film-Noir" 111]

(defn card-genres
  "Returns a map containg the number of films for each genre."
  [archive]
  (reduce (fn [result genre]
            (assoc result genre (count (films-by-genre genre archive)))) {} (all-genres archive)))

(defn max-genre
  [movies-map]
  (reduce (fn [[mx-genre, mx-nb] [genre, nb]]
            (if (> mx-nb nb) [mx-genre mx-nb] [genre, nb])) movies-map))

(defn min-genre
  [movies-map]
  (reduce (fn [[min-genre, min-nb] [genre, nb]]
            (if (< min-nb nb) [min-genre min-nb] [genre, nb])) movies-map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            SECOND PART
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-rating
  "Builds a rating record from a csv entry."
  [[u-id m-id r time]]

  (try
    (let [user-id (Integer/parseInt u-id)
          movie-id (Integer/parseInt m-id)
          rating (Double/parseDouble r)]
      {user-id {movie-id rating}})
    (catch Exception _ nil)))


(defn rating-map
  "Builds a rating map from a CSV base"
  [csv]
  (reduce (fn [rating-base element]
            (let [rating (parse-rating element)         ;; current rating map {user-id {user-rating}}
                  user-id (key (first rating))          ;; user_id
                  user-rating (second (first rating))   ;; user-rating map {movie-id, rating}
                  user-entry (get rating-base user-id)] ;; entries in the databse for this user_id
              (if user-entry               
                (assoc rating-base user-id (into user-entry user-rating))  ;; add to existing
                (into rating-base rating)))) {} csv ))                     ;; create a new entry


(def rating-filename "resources/ml-latest-small/ratings.csv")
(def ratings (rating-map (rest (csv-seq rating-filename))))

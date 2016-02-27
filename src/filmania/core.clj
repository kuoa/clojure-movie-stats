(ns filmania.core
  (require [clojure.data.csv :as csv]
           [clojure.java.io :as io]
           [clojure.pprint :as print]))


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


(def movie-filename "resources/ml-latest-small/movies.csv")

(def movies (movie-map (rest (csv-seq movie-filename))))

(def small-movies (take 30 movies))
(def smaller-movies (take 5 movies))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             FIST PART
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(def card-genre (card-genres movies))

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
(def rating-csv (rest (csv-seq rating-filename)))
(def ratings (rating-map rating-csv))


(defn update-movie-average
  "Updates the entries in the movies-avg-map based on the user-ratings.
  The movies-avg map has this form {movie-id [sum-of-ratings, nb-of-ratings]}.
  For each rating this user has given, the map is updated."

  [movies-avg-map user-ratings]
  (reduce
   (fn [movies-avg, [movie-id, movie-rating]]
     (let [movie-entry (get movies-avg movie-id)]
       (if movie-entry
         (assoc movies-avg movie-id [(+ (first movie-entry) movie-rating) (inc (second movie-entry))])
         (assoc movies-avg movie-id [movie-rating, 1]))))
   movies-avg-map user-ratings))

(defn movies-ratings-count
  "Returns the rating count of all the movies,
  in one single sweep of the ratings map."
  [ratings]
  (reduce
   (fn [movies-avg-map [_ user-ratings-map]] (update-movie-average movies-avg-map user-ratings-map))
   {} ratings))

(defn movie-avg-rating
  "Sweep the movies-ratings-count and compute the average for all the movies."
  [ratings]
  (reduce (fn [movie-average, [movie-id [rating nb-votes]]]
            (assoc movie-average movie-id (/ rating nb-votes))) {} (movies-ratings-count ratings)))


(def movie-avg-ratings (movie-avg-rating ratings))
(def movie-count (count movie-avg-ratings))

(def sorted-movie-avg-rating (sort-by last (movie-avg-rating ratings)))

(defn movie-names-from-ratings
  "Returns a string containing the movie titles from movies-ratings."
  [movies-ratings]
  (reduce
   (fn [movie-print [movie-id, rating]]
     (str movie-print "[[\"title\" \"" (:title (get movies movie-id)) "\"" "] [\"rating\" " rating "]]\n"))
   "" movies-ratings))

(defn best-rated-movies
  "Returns a string containing the n best rated movies"
  [n]
  (str "[\"category\" \"best rated movies \"]\n" (movie-names-from-ratings (take-last n sorted-movie-avg-rating))))

(defn worst-rated-movies
  "Returns a string containing the n best rated movies"
  [n]
  (str "[\"category\" \"worst rated movies \"]\n" (movie-names-from-ratings (take n sorted-movie-avg-rating))))

(def average-rating
  (/ (reduce
      (fn [sum, [_ rating]] (+ sum rating)) 0 movie-avg-ratings) movie-count))

(defn user-avg-rating
  "Returns a map containing the user id and his average rating."  
  [id ratings]
  {id (/ (reduce (fn [sum [_, rating]] (+ sum rating)) 0 ratings) (count ratings))})

(defn users-avg-rating
  "Computes the avgerage rating offered by each user."
  [ratings]
  (reduce (fn [rez [id ratings]] (into rez (user-avg-rating id ratings))) {} ratings))

(def sorted-users-avg-rating (sort-by last (users-avg-rating ratings)))

(defn users-id-from-ratings
  "Returns a string containing the user id from user-map."
  [user-map]
  (reduce
   (fn [user-list [user-id avg-rating]]
     (str user-list "[[\"user-id\" " user-id "] [\"average rating\"" (format "%.2f" avg-rating) "]]\n")) "" user-map))

(defn kindest-users
  "Returns a string containing the users with the highest avg rating in ascending order."
  [n]
  (str "[\"category\" \"highest rating users \"]\n"  (users-id-from-ratings (take-last n sorted-users-avg-rating))))

(defn meanest-users
  "Returns a string containing the users with the lowest avg rating in ascending order."
  [n]
    (str "[\"category\" \"lowest rating users \"]\n"(users-id-from-ratings (take n sorted-users-avg-rating))))

(defn rate-all-movies-from-category
  "Returns a vector of movie titles and their rating, for a specific category."
  [category movies ratings]
  (let [sorted-movies (films-by-genre category movies)]
    (sort-by
     last
     (reduce
      (fn [rezult movie]
        (conj rezult [(:title (second movie)) (get ratings (first movie))])) [] sorted-movies))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             STATS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def line "_______________________________________________")
(def n "\n")
(defn print-line [] (println (str line n)))

(defn surround-text [txt]
  (str line n n txt n line n))

(defn print-hello-msg [] (println (surround-text "         Welcome ^_^")))
(defn print-bbye-msg [] (println (surround-text "         Bye Bye ^_^")))

(defn print-count-stats []
  (print/pprint
   [["movies" (count movies)]
    ["ratings" (count rating-csv)]
    ["users" (count ratings)]]))

(defn print-genres-stats []
  (print/pprint (sort card-genre )))

(defn print-min-genre []
  (print/pprint   
   ["least films" (min-genre card-genre)]))

(defn print-max-genre []
  (print/pprint
   ["most films" (max-genre card-genre)]))

(defn print-average-rating []
  (print/pprint ["average rating" (format "%.2f" average-rating)]))

(defn top-movies-category [n]
  (let [sorted-genres (sort (all-genres movies))]
    (print/pprint
     (reduce
      (fn [rezult genre]
        (assoc
         rezult genre
         (take-last
          n (rate-all-movies-from-category
             genre movies movie-avg-ratings)))) {} sorted-genres))))

(defn print-top-movies-category [n]
  (do (println "[highest rated filmes by category]")
      (top-movies-category n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main
  "Main program"
  [& args]
  (do
    (println)
    (print-hello-msg)      
    (print-count-stats)
    (print-line)
    (print-genres-stats)
    (print-line)
    (print-min-genre)
    (print-max-genre)
    (print-line)
    (print-average-rating)
    (print-line)
    (println (best-rated-movies 20))
    (print-line)
    (println (worst-rated-movies 20))
    (print-line)
    (println (kindest-users 20))
    (print-line)
    (println (meanest-users 20))
    (print-line)
    (print-top-movies-category 10)
    (print-bbye-msg)))

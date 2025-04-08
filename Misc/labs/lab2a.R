library(tidyverse)
song_length <- c(227527, 174000, 160239, 214405, 212000, 173381, 96825, 194050, 238805, 179720, 136760, 185600, 186173, 161840, 168873, 261818, 188918, 187111, 202133, 153190, 204316, 136266, 160656, 152137, 168601, 278440, 225664, 258799, 216764, 207065, 254181, 203807, 175163, 224773, 167480, 153000, 242965, 207853, 221693, 185680, 263288, 186677, 200547, 193346, 186538, 214613, 216120, 165760, 206772, 206385, 174680, 173549, 162680, 169237, 197442, 193279, 231832, 213718, 141805, 215281, 164818, 175344, 214416)

print(paste("maximum:",max(song_length)))
print(paste("minimum:",min(song_length)))
print(paste("length:", (length(song_length))))
print(paste("Sum:",sum(song_length)))


song_popularity <- c(68, 76, 90, 77, 95, 91, 81, 90, 95, 85, 96, 76, 84, 93, 77, 89, 85, 88, 85, 92, 89, 76, 94, 96, 91, 79, 74, 76, 83, 81, 85, 83, 92, 97, 89, 70, 87, 95, 93, 85, 87, 75, 85, 67, 75, 93, 33, 71, 92, 91, 71, 94, 90, 83, 79, 70, 96, 93, 87, 98, 75, 80, 92, 59, 86, 74, 96, 80, 86, 83, 67, 74, 76, 75, 74, 63, 90, 95)
song_changed <- song_popularity[37:52]

print("Next Question")
print(song_changed)
print(paste("maximum:",max(song_changed)))
print(paste("minimum:",min(song_changed)))

song_popularity <- c(76, 86, 82, 89, 85, 70, 83, 77, 74, 81, 95, 83, 77, 33, 94, 83, 87, 94, 87, 83, 59, 96, 74, 76, 75, 95, 92, 93, 97, 92, 85, 76, 79, 75, 71, 91, 85, 71, 76, 81, 96, 80, 80, 90, 85, 67, 89, 93, 85, 90, 74, 85, 73, 92, 96, 98, 88, 79, 74, 96, 75, 93, 90, 87, 67, 75, 84, 74, 68, 85, 83, 88, 90, 87, 92, 95)
print(song_popularity)
print(sum(song_popularity * 4))
print(sum(song_popularity + 10))
print(sum(song_popularity - 9))
print(sum(song_popularity / 2))

song_length <- c(212878, 263288, 203807, 165772, 174680, 200547, 225664, 173549, 231832, 153190, 187943, 189560, 167480, 214405, 162680, 176146, 212000, 238805, 168601, 174000, 202735, 186538, 133613, 216120, 214416, 185680, 213493, 195013, 173381, 278440, 175163, 270586, 179426, 157890, 224694, 206385, 254181, 161266, 216764, 226088, 257213, 160656, 198324, 141805, 210560, 188918, 227527, 165760, 256000, 153000, 215281, 175344, 164818, 96825, 179720, 145800, 172626)
print(song_length)
sum(song_length / (1000*60*60)) %>% 
  round(1) %>% 
    print()

song_title <- c("MAMIII", "Smokin Out The Window", "Beers On Me", "Closer (feat. H.E.R.)", "Heat Waves", "Usain Boo", "Buy Dirt", "Numb Little Bug", "You Right", "Me or Sum (feat. Future & Lil Baby)", "I Love You So", "Come Back As A Country Boy", "Save Your Tears (with Ariana Grande) (Remix)", "'Til You Can't", "Peru", "Don't Play That", "Dos Oruguitas", "I Wish", "THATS WHAT I WANT", "Scorpio", "pushin P (feat. Young Thug)", "Nail Tech", "Bad Habits", "Flower Shops (feat. Morgan Wallen)", "One Right Now (with The Weeknd)", "Easy On Me", "You Should Probably Leave", "The Family Madrigal", "Slow Down Summer")
print(song_title)
song_index <- 9
paste("The title of the song in position", song_index , "is", song_title[song_index])


song_title <- c("I'm Tired - From 'Euphoria' An HBO Original Series", "Cold Heart - PNAU Remix", "MAMIII", "I Hate U", "The Joker And The Queen (feat. Taylor Swift)", "Cigarettes", "INDUSTRY BABY (feat. Jack Harlow)", "Big Energy", "Better Days (NEIKED x Mae Muller x Polo G)", "High", "Bad Habits", "pushin P (feat. Young Thug)", "Need to Know", "Usain Boo", "I Hate YoungBoy", "half of my hometown (feat. Kenny Chesney)", "Knife Talk (with 21 Savage ft. Project Pat)", "Broadway Girls (feat. Morgan Wallen)", "The Motto", "Beautiful Lies", "Iffy", "P power (feat. Drake)", "Don't Play That")
print(song_title)


song_nchar <- nchar(song_title) 

print(paste("maximum:",max(song_nchar)))
print(paste("minimum:",min(song_nchar)))
print(paste("length:", (length(song_nchar))))
print(paste("Sum:",sum(song_nchar)))
print(paste("pos:",sum(song_nchar[23])))

song_title <- c("Circles Around This Town", "Numb Little Bug", "Freaky Deaky", "Bussin", "Dos Oruguitas", "I Hate U", "23", "Usain Boo")
song_popularity <- c(76, 89, 85, 83, 86, 90, 77, 74)
song_length <- c(195760, 169237, 215281, 136760, 214613, 174000, 179720, 186677)

print(song_title)


print(tibble(song_title, song_popularity, song_length))


song_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS7R6DHE_92iP3XMxWScK4fuHfomugS3IKXz4SEDhPi_8kwhUyqJTKAKm1byjHCEKRVnh-Y2mTG9RkH/pub?gid=1306618861&single=true&output=csv")

#song_data %>% View()


song_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS7R6DHE_92iP3XMxWScK4fuHfomugS3IKXz4SEDhPi_8kwhUyqJTKAKm1byjHCEKRVnh-Y2mTG9RkH/pub?gid=2038908886&single=true&output=csv")
#song_data %>% View()

song_popularity <- song_data$song_popularity
song_length <- song_data$song_length
song_title <- song_data$song_title
print(sum(song_popularity * 200))
print(max(song_length))
print(song_title[12] %>% nchar())
print(min(song_popularity))

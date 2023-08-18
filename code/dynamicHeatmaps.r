###########################################################
#       Script for Naukas23 cell animated figures         #
###########################################################

#------ Loading libraries.
library(ggplot2)
library(gganimate)
library(reshape)
library(wacolors)
library(gtools)

#------ Set working directory
setwd("~/divulgacion/Naukas23/Rscript")

#------ Set seed for reproduce random numbers
set.seed(123)

#------ Set variables
nframes <- 10
times <- c( 0,  1,  2,  4,  8, 
           12, 18, 24, 36, 48)

#------ Prepare an initial bimodal skewed matrix
m <- matrix(permute(c(round(abs(rbeta(50, 1, 4)), 2),
                      round(abs(rbeta(50, 4, 1)), 2))),
              10, 10)


#------ Transform the matrix in long format
df <- melt(m)
colnames(df) <- c("x", "y", "value")
df$time <- times[1]
original.df <- df

#################################################
# Live/dead 
#################################################
my.df <- original.df
png("vivo.png", width=1000, height=1000, res=150)
p <- ggplot(my.df, aes(x, y, fill=value)) +
  geom_tile(color = "white",lwd = 0.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_wa_c("ferries") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") + 
  labs(title = "Vivo")    + 
  theme(plot.title = element_text(size = 30)) +
  # Remove external grey area
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) 
print(p)
dev.off()

my.df <- original.df
my.df$value <- 0
png("muerto.png", width=1000, height=1000, res=150)
p <- ggplot(my.df, aes(x, y)) +
  geom_tile(color = "white",lwd = 0.5,
            linetype = 1, fill="black") +
  coord_fixed() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") + 
  labs(title = "Muerto")    + 
  theme(plot.title = element_text(size = 30)) +
  # Remove external grey area
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) 
print(p)
dev.off()




#------ Construct the rest of frames
for(i in 2:nframes) {
  my.df <- original.df
  my.df$time <- times[i]
  my.df$value <- my.df$value*0.92^(i*sqrt(i)*log2(times[i]))
  df <- rbind(df, my.df)
}

# Make a ggplot
p <- ggplot(df, aes(x, y, fill=value)) +
  geom_tile(color = "white",lwd = 0.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_wa_c("ferries") +
  transition_time(time, range = c(1, nframes)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") + 
  labs(title = "{times[frame_time]}h")    + 
  theme(plot.title = element_text(size = 80)) +
  # Remove external grey area
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  ease_aes('linear')

# Save at gif:
anim_save("heatmap0.gif", p, nframes=nframes, fps=0.75, 
          width = 1000, height = 1000,
          renderer = gifski_renderer(loop = FALSE))


#############################################################
#                Relive cells
#############################################################
df <- original.df
nframes <- 5
times <- 1:5
#------ Construct the rest of frames
for(i in 2:(nframes)) {
  my.df <- original.df
  my.df$time <- times[i]
  my.df$value <- my.df$value*0.95^(i*log2(times[i]))
  df <- rbind(df, my.df)
}

# Make a ggplot
p <- ggplot(df, aes(x, y, fill=value)) +
  geom_tile(color = "white",lwd = 0.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_wa_c("ferries") +
  transition_time(time, range = c(1, nframes)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") + 
  labs(title = "{times[frame_time]}h")    + 
  theme(plot.title = element_text(size = 80)) +
  # Remove external grey area
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  ease_aes('linear')

# Save at gif:
anim_save("heatmap1.gif", p, nframes=nframes, fps=0.75, 
          width = 1000, height = 1000,
          renderer = gifski_renderer(loop = FALSE))

##########################################
# New cells
##########################################
original.df <- my.df
df <- original.df

df[df$x==8 & df$y==10, ]$value <- 1
df[df$x==7 & df$y==9, ]$value <- 1
df[df$x==8 & df$y==9, ]$value <- 1
df[df$x==9 & df$y==9, ]$value <- 1
df[df$x==8 & df$y==8, ]$value <- 1
df$time <- 6
original.df <- df

times <- c(6,7,8,9,10)

for(i in 2:(nframes)) {
  my.df <- original.df
  my.df$time <- times[i]
  my.df$value <- my.df$value*0.95^(i*log2(times[i]-6))
  df <- rbind(df, my.df)
}

# Make a ggplot
p <- ggplot(df, aes(x, y, fill=value)) +
  geom_tile(color = "white",lwd = 0.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_wa_c("ferries") +
  transition_time(time) + #, range = c(6,10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") + 
  labs(title = "{frame_time}h")    + 
  theme(plot.title = element_text(size = 80)) +
  # Remove external grey area
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  ease_aes('linear')

# Save at gif:
anim_save("heatmap2.gif", p, nframes=nframes, fps=0.75, 
          width = 1000, height = 1000,
          renderer = gifski_renderer(loop = FALSE))

#########################################################
original.df <- my.df
df <- original.df

df[df$x==2 & df$y==2, ]$value <- 1
df[df$x==2 & df$y==3, ]$value <- 1
df[df$x==2 & df$y==4, ]$value <- 1
df[df$x==3 & df$y==3, ]$value <- 1
df[df$x==4 & df$y==4, ]$value <- 1
df$time <- 11
original.df <- df

times <- c(11,12,13,14,15)

for(i in 2:(nframes)) {
  my.df <- original.df
  my.df$time <- times[i]
  my.df$value <- my.df$value*0.95^(i*log2(times[i]-11))
  df <- rbind(df, my.df)
}

# Make a ggplot
p <- ggplot(df, aes(x, y, fill=value)) +
  geom_tile(color = "white",lwd = 0.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_wa_c("ferries") +
  transition_time(time) + #, range = c(6,10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") + 
  labs(title = "{frame_time}h")    + 
  theme(plot.title = element_text(size = 80)) +
  # Remove external grey area
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  ease_aes('linear')

# Save at gif:
anim_save("heatmap3.gif", p, nframes=nframes, fps=0.75, 
          width = 1000, height = 1000,
          renderer = gifski_renderer(loop = FALSE))


original.df <- my.df
df <- original.df

df[df$x==7 & df$y==6, ]$value <- 1
df[df$x==7 & df$y==5, ]$value <- 1
df[df$x==8 & df$y==6, ]$value <- 1
df[df$x==8 & df$y==5, ]$value <- 1
df[df$x==8 & df$y==4, ]$value <- 1
df$time <- 16
original.df <- df

times <- c(16,17,18,19,20)

for(i in 2:(nframes)) {
  my.df <- original.df
  my.df$time <- times[i]
  my.df$value <- my.df$value*0.95^(i*log2(times[i]-16))
  df <- rbind(df, my.df)
}

# Make a ggplot
p <- ggplot(df, aes(x, y, fill=value)) +
  geom_tile(color = "white",lwd = 0.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_wa_c("ferries") +
  transition_time(time) + #, range = c(6,10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") + 
  labs(title = "{frame_time}h")    + 
  theme(plot.title = element_text(size = 80)) +
  # Remove external grey area
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  ease_aes('linear')

# Save at gif:
anim_save("heatmap4.gif", p, nframes=nframes, fps=0.75, 
          width = 1000, height = 1000,
          renderer = gifski_renderer(loop = FALSE))


original.df <- my.df
df <- original.df

df[df$x==2 & df$y==9, ]$value <- 1
df[df$x==3 & df$y==9, ]$value <- 1
df[df$x==3 & df$y==8, ]$value <- 1
df[df$x==4 & df$y==8, ]$value <- 1
df[df$x==4 & df$y==7, ]$value <- 1
df$time <- 21
original.df <- df

times <- c(21,22,23,24,25)

for(i in 2:(nframes)) {
  my.df <- original.df
  my.df$time <- times[i]
  my.df$value <- my.df$value*0.95^(i*log2(times[i]-21))
  df <- rbind(df, my.df)
}

# Make a ggplot
p <- ggplot(df, aes(x, y, fill=value)) +
  geom_tile(color = "white",lwd = 0.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_wa_c("ferries") +
  transition_time(time) + #, range = c(6,10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") + 
  labs(title = "{frame_time}h")    + 
  theme(plot.title = element_text(size = 80)) +
  # Remove external grey area
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  ease_aes('linear')

# Save at gif:
anim_save("heatmap5.gif", p, nframes=nframes, fps=0.75, 
          width = 1000, height = 1000,
          renderer = gifski_renderer(loop = FALSE))


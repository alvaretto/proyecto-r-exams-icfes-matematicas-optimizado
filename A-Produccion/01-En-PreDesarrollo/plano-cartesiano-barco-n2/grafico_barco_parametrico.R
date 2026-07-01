library(ggplot2)

# --- Parameters (randomized in .Rmd) ---
x_min <- 4; x_max <- 9; y_min <- 5; y_max <- 6
grid_max <- 10

# Derived
cx <- (x_min + x_max) / 2
cy <- (y_min + y_max) / 2
w  <- x_max - x_min   # total width
h  <- y_max - y_min    # total height (must FILL from y_min to y_max)

# ======================================================================
# BOAT HULL: pointed bow (left = x_min), blunt stern (right = x_max)
# The hull MUST touch y_min and y_max so the student reads the bounding box.
# ======================================================================
n_pts <- 400

# -- Top edge: from bow (x_min, cy) curving up to touch y_max, then to stern (x_max, cy)
# -- Bottom edge: from stern (x_max, cy) curving down to touch y_min, then to bow
# We build parametrically: t in [0,1] maps x from x_min to x_max

t_fwd <- seq(0, 1, length.out = n_pts)  # bow -> stern

# x is simply linear
x_top <- x_min + t_fwd * w
x_bot <- x_max - t_fwd * w   # stern -> bow (reversed)

# y profile: bow is pointed (y = cy at t=0), hull widens to full h/2,
# then stern is blunt (drops abruptly back to cy)
# Use a piecewise profile:
#   t in [0, 0.15]: bow taper  (quick rise from 0 to ~0.95*h/2)
#   t in [0.15, 0.85]: full width (stays at h/2 = touches y_min/y_max)
#   t in [0.85, 1.0]: stern taper (drops back to cy, but less sharp than bow)

profile_top <- function(t) {
  ifelse(t < 0.15,
    (h/2) * (t / 0.15)^0.7,            # bow: rises quickly
    ifelse(t < 0.85,
      h/2,                               # midship: full width (touches y_max)
      (h/2) * ((1 - t) / 0.15)^0.5       # stern: drops back, but more gently
    )
  )
}

y_top <- cy + profile_top(t_fwd)
y_bot <- cy - profile_top(t_fwd)  # mirror for bottom

# Combine into closed polygon (top edge forward, then bottom edge backward)
hull_x <- c(x_top, x_bot)
hull_y <- c(y_top, y_bot)
hull_df <- data.frame(x = hull_x, y = hull_y)

# ======================================================================
# INNER HULL LINE (slightly inset, lighter gray)
# ======================================================================
inset_x <- w * 0.03
inset_y <- h * 0.10

profile_inner <- function(t) {
  ifelse(t < 0.18,
    ((h/2) - inset_y) * (t / 0.18)^0.7,
    ifelse(t < 0.82,
      (h/2) - inset_y,
      ((h/2) - inset_y) * ((1 - t) / 0.18)^0.5
    )
  )
}

inner_x_top <- (x_min + inset_x) + t_fwd * (w - 2*inset_x)
inner_y_top <- cy + profile_inner(t_fwd)
inner_x_bot <- rev(inner_x_top)
inner_y_bot <- cy - profile_inner(rev(t_fwd))

inner_df <- data.frame(
  x = c(inner_x_top, inner_x_bot),
  y = c(inner_y_top, inner_y_bot)
)

# ======================================================================
# PORTHOLES (6 dots, 2 columns of 3)
# ======================================================================
ph_col1_x <- cx - w * 0.18
ph_col2_x <- cx - w * 0.12
ph_dy <- h * 0.20
ph_df <- data.frame(
  x = c(ph_col1_x, ph_col1_x, ph_col1_x, ph_col2_x, ph_col2_x, ph_col2_x),
  y = c(cy + ph_dy, cy, cy - ph_dy, cy + ph_dy, cy, cy - ph_dy)
)

# ======================================================================
# DARK CURVED BAND 1 (mid-ship, crescent shape)
# ======================================================================
band1_cx <- cx + w * 0.03
band1_r_out <- h * 0.46
band1_r_in  <- h * 0.34
band1_t <- seq(-1.1, 1.1, length.out = 80)  # radians, ~63 degrees each side

band1_df <- data.frame(
  x = c(band1_cx + band1_r_out * cos(band1_t),
        band1_cx + band1_r_in  * cos(rev(band1_t))),
  y = c(cy + band1_r_out * sin(band1_t),
        cy + band1_r_in  * sin(rev(band1_t)))
)

# ======================================================================
# DARK CURVED BAND 2 (smaller, between band1 and bridge)
# ======================================================================
band2_cx <- cx + w * 0.16
band2_r_out <- h * 0.40
band2_r_in  <- h * 0.30
band2_t <- seq(-0.9, 0.9, length.out = 60)

band2_df <- data.frame(
  x = c(band2_cx + band2_r_out * cos(band2_t),
        band2_cx + band2_r_in  * cos(rev(band2_t))),
  y = c(cy + band2_r_out * sin(band2_t),
        cy + band2_r_in  * sin(rev(band2_t)))
)

# ======================================================================
# BRIDGE / CABIN (filled dark rectangle near stern)
# ======================================================================
br_w <- w * 0.10
br_h <- h * 0.50
br_cx <- cx + w * 0.32
bridge_df <- data.frame(
  x = c(br_cx - br_w/2, br_cx + br_w/2, br_cx + br_w/2, br_cx - br_w/2),
  y = c(cy - br_h/2,    cy - br_h/2,    cy + br_h/2,    cy + br_h/2)
)

# ======================================================================
# PLOT
# ======================================================================
p <- ggplot() +
  # Grid
  geom_hline(yintercept = 0:grid_max, color = "gray82", linewidth = 0.3) +
  geom_vline(xintercept = 0:grid_max, color = "gray82", linewidth = 0.3) +
  # Axes (thick)
  geom_segment(aes(x = 0, y = 0, xend = grid_max + 0.4, yend = 0), linewidth = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = grid_max + 0.4), linewidth = 1.5) +
  # Boat hull (thick dark border, white fill)
  geom_polygon(data = hull_df, aes(x, y),
               fill = "gray96", color = "black", linewidth = 2.0) +
  # Inner hull line
  geom_polygon(data = inner_df, aes(x, y),
               fill = NA, color = "gray60", linewidth = 0.4) +
  # Dark band 1
  geom_polygon(data = band1_df, aes(x, y), fill = "gray15", color = NA) +
  # Dark band 2
  geom_polygon(data = band2_df, aes(x, y), fill = "gray15", color = NA) +
  # Bridge (filled dark)
  geom_polygon(data = bridge_df, aes(x, y),
               fill = "gray15", color = "black", linewidth = 0.6) +
  # Portholes
  geom_point(data = ph_df, aes(x, y), size = 2.0, shape = 16, color = "black") +
  # Axis ticks + labels
  scale_x_continuous(breaks = 1:grid_max,
                     limits = c(-0.5, grid_max + 0.7), expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:grid_max,
                     limits = c(-0.5, grid_max + 0.7), expand = c(0, 0)) +
  # Axis titles
  annotate("text", x = grid_max + 0.55, y = -0.25, label = "x",
           fontface = "bold", size = 6) +
  annotate("text", x = -0.30, y = grid_max + 0.55, label = "y",
           fontface = "bold", size = 6) +
  coord_fixed() +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid   = element_blank(),
    axis.text    = element_text(size = 13, color = "black", face = "bold"),
    axis.title   = element_blank(),
    plot.margin  = margin(8, 12, 8, 8)
  )

out <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(out)) out <- "boat_r_v2.png"
ggsave(out, p, width = 7, height = 7, dpi = 150, bg = "white")
cat("R v2 saved to", out, "\n")

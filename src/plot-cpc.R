library(ggplot2)
library(sf)

cpc <- read_sf(here::here("scratch/cz_cpc_measures.geojson"))
cpc <- cpc |>
  dplyr::mutate(dplyr::across(gravity_rmse:rad_rmse, log)) |>
  tidyr::pivot_longer(
  cols = gravity_cpc:rad_rmse,
  names_to = "model",
  values_to = "measure"
)
cpc <- cpc |>
  dplyr::mutate(
    model = dplyr::case_when(
      model == "gravity_cpc" ~ "Gravity (CPC: 0.301)",
      model == "rad_cpc" ~ "Radiation (CPC: 0.202)",
      model == "gravity_rmse" ~ "Gravity (RMSE: 3,260)",
      model == "rad_rmse" ~ "Radiation (RMSE: 4,712)"
    )
  )

# Plot it!
cpc_plot <- cpc |>
  tigris::shift_geometry() |>
  dplyr::filter(startsWith(model, "Gravity (C") | startsWith(model, "Radiation (C")) |>
  ggplot(aes(fill = measure)) +
  geom_sf() +
  facet_wrap(~ model, nrow = 1) +
  viridis::scale_fill_viridis() +
  labs(fill = "CPC") +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.border = element_rect(color = "black", fill = NA)
  )

rmse_plot <- cpc |>
  tigris::shift_geometry() |>
  dplyr::filter(startsWith(model, "Gravity (R") | startsWith(model, "Radiation (R")) |>
  ggplot(aes(fill = measure)) +
  geom_sf() +
  facet_wrap(~ model, nrow = 1) +
  viridis::scale_fill_viridis(direction = -1) +
  labs(fill = "log(RMSE)") +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.border = element_rect(color = "black", fill = NA)
  )

cowplot::plot_grid(cpc_plot, rmse_plot, nrow = 2, align = "hv")

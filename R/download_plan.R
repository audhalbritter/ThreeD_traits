#download_data
download_plan <- list(

  # vegetation
  tar_target(
    name = cover_download,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_clean_cover_2019-2022.csv",
                        path = "data",
                        remote_path = "Vegetation"),
  format = "file"
  ),

  # traits
  tar_target(
    name = trait_download,
    command =  get_file(node = "fcbw4",
                        file = "PFTC6_ThreeD_clean_leaf_traits_2022.csv",
                        path = "data",
                        remote_path = "i. trait_data"),
    format = "file"
  ),

  # slope and aspect
  tar_target(
    name = slope_download,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_slope_aspect_soil_depth_2019.csv",
                        path = "data",
                        remote_path = "Site"),
    format = "file"
  ),

  # climate
  tar_target(
    name = climate_download,
    command =  get_file(node = "pk4bg",
                        file = "THREE-D_clean_microclimate_2019-2022.csv.zip",
                        path = "data",
                        remote_path = "Climate")
  ),

  # unzip and delete zip file
  tar_target(
    name = climate_unzip,
    command =  {
      unzip(climate_download, exdir = "data")

      zip <- "data/THREE-D_clean_microclimate_2019-2022.csv.zip"
      unzip <- "data/THREE-D_clean_microclimate_2019-2022.csv"
      #Check its existence
      if (file.exists(zip) & file.exists(unzip)) {
        #Delete file if it exists
        file.remove(zip)
        }
      },
    format = "file"
  ),

  # meta data
  tar_target(
    name = metaTurfID,
    command = create_threed_meta_data()
  ),

  # slope
  tar_target(
    name = slope_raw,
    command = read_csv(slope_download)
  ),

  # climate
  tar_target(
    name = climate_raw,
    command = read_csv(climate_unzip)
  ),

  # cover
  tar_target(
    name = cover_raw,
    command = read_csv(cover_download)
  ),

  # sp list
  tar_target(
    name = sp_list,
    command = read_csv(file = "data/Three-D_clean_taxonomy.csv")
  ),

  # community structure
  tar_target(
    name = trait_raw,
    #command = read_csv(trait_download)
    command = read_csv(file = "data/PFTC6_ThreeD_clean_leaf_traits_2022.csv")
  )

)


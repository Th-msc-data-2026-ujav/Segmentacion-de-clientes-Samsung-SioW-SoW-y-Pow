#-----------------------------------------------------------------------------
# Exportar IDs de entrenamiento y testeo
#-----------------------------------------------------------------------------
# Este bloque guarda en archivos CSV los identificadores únicos de los clientes
# asignados al conjunto de entrenamiento y al conjunto de testeo del modelo Pogit.
#
# La exportación de estos IDs permite reutilizar exactamente la misma partición
# train/test en otros análisis posteriores, como la segmentación de clientes.
# Esto es importante porque garantiza comparabilidad metodológica entre el modelo
# Pogit y los modelos de agrupamiento: los clústeres se construyen usando solo
# clientes de entrenamiento y luego pueden evaluarse sobre clientes de testeo,
# respetando la misma división de datos y evitando fuga de información.
#-----------------------------------------------------------------------------

readr::write_csv(
  tibble::tibble(response_id = train_ids),
  "train_ids.csv"
)

readr::write_csv(
  tibble::tibble(response_id = test_ids),
  "test_ids.csv"
)
# Copyright 2024 Matěj Kolouch Grabovský
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(rsconnect)

get_env <- function(name, default = NA) {
  value <- Sys.getenv(name, unset = default)

  if (is.na(value)) {
    stop(glue::glue("Environment variable {name} not defined"), call. = FALSE)
  }

  return(value)
}

setAccountInfo(
  name = get_env("SHINY_ACCOUNT_NAME"),
  secret = get_env("SHINY_SECRET"),
  token = get_env("SHINY_TOKEN")
)

deployApp(
  appFiles = c("app.r", "entsoe-cz.parquet"),
  appName = get_env("SHINY_APP_NAME"),
  appTitle = "Explorace variability výroby z OZE",
  forceUpdate = TRUE
)


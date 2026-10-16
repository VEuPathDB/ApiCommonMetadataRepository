pfal_dir <- function() file.path(REPO_ROOT, 'stf', 'studies', 'PlasmoDB', 'pfal3D7')
real_rules <- function() file.path(REPO_ROOT, 'stf', 'harmonization', 'rules')

test_that("pfal3D7 harmonizes to the expected value collapses", {
  skip_if_not(dir.exists(pfal_dir()), 'pfal3D7 study not present')
  out_stf <- file.path(tempdir(), 'pfal_stf'); out_rep <- file.path(tempdir(), 'pfal_rep')
  res <- harmonize_one_study(pfal_dir(), 'PlasmoDB', 'pfal3D7', real_rules(),
                             out_stf, out_rep)
  expect_equal(res$status, 'ok')

  e <- get_entity(study_from_stf(file.path(out_stf, 'PlasmoDB', 'pfal3D7')), 'sample')
  expect_true(validate(quiet(e)))
  d <- get_data(e)

  # organism renamed and collapsed: 327 'Homo sapiens' + 194 'human' = 521
  expect_true('host_organism' %in% colnames(d))
  expect_false('organism' %in% colnames(d))
  expect_setequal(na.omit(unique(d$host_organism)), 'Homo sapiens')
  expect_equal(sum(d$host_organism == 'Homo sapiens', na.rm = TRUE), 521)

  # display name set, provenance preserved
  md <- get_variable_metadata(e) %>% filter(variable == 'host_organism')
  expect_equal(md$display_name, 'Host organism')
  expect_equal(unlist(md$provider_label), 'organism')

  # specimen: 180 'Blood' + 116 'blood' = 296
  expect_equal(sum(d$specimen == 'blood', na.rm = TRUE), 296)
  expect_false('Blood' %in% d$specimen)

  # parasite_organism collapsed to species
  expect_setequal(na.omit(unique(d$parasite_organism)), 'Plasmodium falciparum')

  # continent dropped by approved-drop authorisation
  expect_false('continent' %in% colnames(d))

  # every polluted country value resolved to Thailand
  expect_false(any(grepl(',', d$country), na.rm = TRUE))
  expect_false(any(c('PLU', 'MRH') %in% d$country))

  # row count unchanged: harmonization never adds or removes samples
  expect_equal(nrow(d), 537)
})

test_that("pfal3D7 structural findings still report the deferred defects", {
  skip_if_not(dir.exists(pfal_dir()), 'pfal3D7 study not present')
  e <- get_entity(study_from_stf(pfal_dir()), 'sample')
  f <- structural_findings(e, c('dataset_id', 'sample_display_name'))
  expect_true('Wang Pha, Thailand' %in%
                (f %>% filter(check == 'country_pollution') %>% pull(value)))
  expect_true('parasite_strain' %in%
                (f %>% filter(check == 'identifier_like') %>% pull(variable)))
})

test_that("harmonizing pfal3D7 never modifies its input", {
  skip_if_not(dir.exists(pfal_dir()), 'pfal3D7 study not present')
  before <- tools::md5sum(list.files(pfal_dir(), full.names = TRUE))
  harmonize_one_study(pfal_dir(), 'PlasmoDB', 'pfal3D7', real_rules(),
                      file.path(tempdir(), 'pfal_ro_stf'),
                      file.path(tempdir(), 'pfal_ro_rep'))
  expect_equal(tools::md5sum(list.files(pfal_dir(), full.names = TRUE)), before)
})

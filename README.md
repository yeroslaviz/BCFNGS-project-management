# BCFNGS-project-management

# Welcome to the Sequencing Facility at MPI for Biochemistry

a shiny app for a project management system for the BCF NGS core facility

The NGS lab of the Core Facility is providing sequencing as a service. We highly recommend to contact us before the start of your experiment. In collaboration with the Bioinformatics Core Facility, we provide:

* Assistance with experimental design of the studies (required number of samples and replicates)
* Assistance with the use of NGS open source analysis tools
* Data analysis on collaborative basis

If you have any further questions, please do not hesitate to [contact us @ omicsdesk](mailto:omicsdesk.biochem.mpg.de)!

# Registration

If you want to use the NGS service for the first time, you have to [register for an account](https://ngs-vm.biochem.mpg.de/register.cgi). Please use your MPIB credentials for account registration.

# Login

After registration is complete, you can directly login with your MPIB account to submit your new project(s).

# Operations docs

- [Git auth on new laptop](README-git-auth-new-laptop.md)
- [VM troubleshooting guide (includes LDAP deploy smoke tests)](notes/vm-troubleshooting-guide.md)
- [Operations booklet landing page (Quarto)](booklet/index.qmd)

## Publish docs on GitHub Pages (Quarto)

The repository now includes a workflow at `.github/workflows/quarto-gh-pages.yml` that builds and publishes the Quarto booklet from:

- all root `.qmd` files
- all files under `notes/*.md`

To publish:

1. Push to `main`
2. In GitHub repository settings, set `Pages -> Build and deployment -> Source` to `GitHub Actions`


# Changes

## 14.11.2026

1. changing admin email address
2. add editing possibilities to the tables containing costs information.
3. bi-weekly backup is added, but still commented out. 
4. list of group leaders can now be read from an Excel sheet (must be in the same folder)

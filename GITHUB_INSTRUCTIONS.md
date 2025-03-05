# GitHub Repository Setup Instructions

Follow these steps to create a new GitHub repository and push your local repository to it:

## 1. Create a new repository on GitHub

1. Go to [GitHub](https://github.com/) and sign in to your account
2. Click on the "+" icon in the top right corner and select "New repository"
3. Name your repository (e.g., "HVI_Johannesburg_Data")
4. Add a description: "Heat Vulnerability Index data sources and analysis for Johannesburg"
5. Choose whether to make the repository public or private
6. Do NOT initialize the repository with a README, .gitignore, or license (since we already have files locally)
7. Click "Create repository"

## 2. Connect your local repository to GitHub

After creating the repository, GitHub will show instructions. Follow the "push an existing repository" instructions.

Run these commands in the data_sources folder:

```
git remote add origin https://github.com/YOUR_USERNAME/HVI_Johannesburg_Data.git
git branch -M main
git push -u origin main
```

Replace `YOUR_USERNAME` with your actual GitHub username.

## 3. Verify the repository

1. Refresh the GitHub page to see your files
2. Make sure all the data files and the reproduce.R script are visible in the repository

Your Heat Vulnerability Index data and analysis script are now available on GitHub!

# Instructions for contributors
BrainEffeX welcomes contributions of large (n>500) subject-level datasets. Data should be provided at the subject level, after preprocessing and computation of functional connectivity or task-based activation. Specifically, a .mat file containing three structs should be provided, with fields listed in Figure 3. Template .mat files with the proper structure for each test type may be downloaded from https://github.com/neuroprismlab/BrainEffeX/tree/main/for_contributors/templates to facilitate the organization of the data files.

If you have any issues while trying to contribute data, please don't hesistate to file an issue here on Github (via a 'Blank' issue request, not the contribution template), or email us at neuroprismlab@gmail.com. We would be happy to walk through any of these steps with you!

**To contribute data to BrainEffeX, please follow these steps...**

## 1. Format your data
Data should be provided as a .mat struct. See the figure below for information about the required fields. Empty template structs for each test type (t, r, t2) are available in BrainEffeX/for_contributors/templates/. 

Save your .mat file in a directory that contains only this .mat file (this is important for uploading the data later) - the name of the directory does not matter. 
![contributor data structure](https://github.com/user-attachments/assets/5657b384-42d9-4e0b-81ee-47aaf8665ce0)


## 2. Upload your data to Brainlife.io
1. Create an account on [Brainlife.io.](https://brainlife.io/auth/#!/signup)
2. Verify your account with your email, and login.
3. Create a project:
   - Go to "projects" near the top left of the page.
   - On the bottom right, click the green "New Project" button.
   - For the project name, please include the name of the dataset, your last name, and "EffeX". For example, "HCP_Shearer_EffeX"
   - Click "Access Control" near the top. Please add us as a 'Guest' with the email address neuroprismlab@gmail.com
   ![Screenshot 2025-01-28 at 1 45 18 PM](https://github.com/user-attachments/assets/d3ac73f5-1618-4f9f-8b97-afcc54aa60b0)
      - NOTE: Sometimes the neuroprismlab account won't show up if you copy-paste the email address. Instead, please type neuroprismlab and the email should show up.
   - Click "Submit" on the bottom right.
4. Upload data
   - On Brainlife, uploaded data should fit a preset data type. By utilizing a datatype, this ensures that the uploaded data fits the requirements of that type of data. Since we are currently only using Brainlife as a means to transfer the data, we have not created a datatype for EffeX contributions on Brainlife (although in the future we hope to transform the analysis pipeline into a series of Brainlife apps). Data that does not fit a datatype can be uploaded as the “raw” datatype that does not impose any restrictions. However, this type of data upload can only be done through the command line. Therefore, the following steps must be completed in the command line (i.e., terminal).
   - Install the Brainlife command line interface following the instructions at https://brainlife.io/docs/cli/install/.
     - for Mac users, you can follow these summarized instructions, but they may become outdated so refer to the official instructions if you encounter issues.
     - run ```brew install node``` to download node
     - run ```sudo npm install -g brainlife``` to install brainlife. Enter your computer's username and password when prompted
   - Login to Brainlife in the command life with ```bl login --ttl 7```, where 7 is the number of days to remain logged in (feel free to change the number).
   - Find your project ID by running ```bl project query --admin {username}```. From the results, find your EffeX contribution project and note down the project ID.
   - To upload your data, run ```bl data upload --datatype raw --project {project_ID} --subject {subject_name} --output {data_dir_path}``` where project_ID is the ID from the previous step, subject_name is 1, and data_dir_path is the path to the data directory where your .mat file is stored (not the full path to the .mat file, just the path of the directory containing the .mat file).
   - Once the upload is complete, you can check that the file was successfully uploaded by returning to brainlife.io. Go to 'projects' on the left navigation bar, then select your data contribution project, and select 'archive' from the top navigation bar. You should see one 'subject' with a datatype 'raw'.
   ![Screenshot 2025-01-28 at 2 12 03 PM](https://github.com/user-attachments/assets/940ffc95-d408-4d10-8441-ea9c184633bf)
  - **Note the URL of this project page for the next step.**
## 3. Submit Github Issue (data contribution request)
  - Navigate to the Github [Issue page](https://github.com/neuroprismlab/BrainEffeX/issues) for BrainEffeX.
  - Near the top right, select the green "New Issue" button.
  - Select "Data Contribution"
  ![Screenshot 2025-01-28 at 3 11 41 PM](https://github.com/user-attachments/assets/0d0261e7-05af-4495-8b52-7a878be55f76)
  - Fill in the form, providing information about the data that you are contributing.
  - In the last field, provide the URL to the Brainlife project where you uploaded the data.
  - Submit the form!



This project benefits from the use of data or technology provided by brainlife.io (NSF BCS 1734853 to Franco Pestilli).

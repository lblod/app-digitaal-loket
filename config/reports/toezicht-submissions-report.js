import generateSubmissionsReport from './toezicht-submissions/submissions';
import generateUploadedFilesReport from './toezicht-submissions/uploaded-files';
import generateNonSentAutomaticSubmissionsReport from './toezicht-submissions/non-sent-automatic-submissions';

const metadata = {
  title: 'Toezicht module: Meldingen',
};

export default {
  cronPattern: '30 22 * * *',
  name: 'toezicht-submissions',
  execute: async () => {
    const startTime = new Date();
    const reportTime = new Date('2022-01-01');
    console.log(
        `[INFO] Starting with reports for [${metadata.title}] @ ${startTime.toISOString()}`);
    await generateSubmissionsReport(reportTime);
    await generateUploadedFilesReport(reportTime);
    await generateNonSentAutomaticSubmissionsReport(reportTime);
    console.log(
        `[INFO] Finished reports for [${metadata.title}] @ ${new Date().toISOString()}. ` +
        `Time elapsed: ${Math.abs(new Date() - startTime)}ms`,
    );
  },
};

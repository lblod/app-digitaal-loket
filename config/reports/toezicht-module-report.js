import generateSubmissionsReport from './toezicht-module/submissions';
import generateUploadedFilesReport from './toezicht-module/uploaded-files';

const metadata = {
  title: 'Toezicht module',
};

export default {
  cronPattern: '0 0 22 * * 6',
  name: 'toezicht-module-report',
  execute: async () => {
    const startTime = new Date();
    const reportTime = new Date('2021-01-01');
    console.log(
        `[INFO] Starting with reports for [${metadata.title}] @ ${startTime.toISOString()}`);
    await generateSubmissionsReport(reportTime);
    await generateUploadedFilesReport(reportTime);
    console.log(
        `[INFO] Finished reports for [${metadata.title}] @ ${new Date().toISOString()}. ` +
        `Time elapsed: ${Math.abs(new Date() - startTime)}ms`,
    );
  },
};
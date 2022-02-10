import generateMeldingenReport from './toezicht/meldingen';
import generateGeuploadeBestandenReport from './toezicht/geuploade-bestanden';

const metadata = {
  title: 'Toezicht Module',
}

export default {
  cronPattern: '0 0 22 * * 7',
  name: 'toezicht',
  execute: async () => {
    const startTime = new Date();
    console.log(
        `[INFO] Starting with reports for [${metadata.title}] @ ${startTime.toISOString()}`);
    await Promise.all([
      generateMeldingenReport(),
      generateGeuploadeBestandenReport()
    ]);
    console.log(
        `[INFO] Finished report for [${metadata.title}] @ ${new Date().toISOString()}. ` +
        `Time elapsed: ${new Date() - startTime}ms`,
    );
  },
};



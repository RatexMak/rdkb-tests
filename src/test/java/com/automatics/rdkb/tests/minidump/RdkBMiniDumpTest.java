/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.minidump;

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.CrashConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ProcessRestartOption;
import com.automatics.enums.StbProcess;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.tests.base.BroadBandMiniDumpBaseTest;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ResultValues;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.utils.CommonMethods;

public class RdkBMiniDumpTest extends BroadBandMiniDumpBaseTest {

    /**
     * This method is written to add mini dumps to S3 amazon locations' .This method tests the generation of minidump
     * when Ccsp process is killed and verifies the minidump upload to: 1.Amazon S3 server with correct S3 Amazon
     * Signing Url 2. Crash portal as fail over mechanism with incorrect S3 Amazon Signing Url
     * 
     * <ol>
     * <li>Step 1: Obtain Pid of CcspPandMSsp process.</li>
     * <li>Step 2: Create a file /nvram/coredump.properties and override the S3_AMAZON_SIGNING_URL</li>
     * <li>Step 3: Kill CcspPandMSsp process and verify it has started again.</li>
     * <li>Step 4: Poll and check core_log.txt for upload failed and retry log messages</li>
     * <li>Step 5: Poll and check for fail over mechanism log message is not present in core_log.txt. If Fail over
     * mechanism fails, need to check for that log message is not present</li>
     * <li>Step 6: Verify minidump is not uploaded to new Failover servers</li>
     * <li>Step 7: Check if minidump folder has been cleaned up and the log messages present.</li>
     * <li>Step 8: Remove /nvram/coredump.properties file</li>
     * <li>Step 9: Obtain Pid of CcspPandMSsp process.</li>
     * <li>Step 10: Kill CcspPandMSsp process and verify it has started again.</li>
     * <li>Step 11: Poll and check for S3 Amazon upload success log message in core_log.txt</li>
     * <li>Step 12: Check if minidump folder has been cleaned up and the log messages present.</li>
     * </ol>
     * 
     * @author Ashwin Sankarasubramanian
     * @refactor Said Hisham
     * 
     * @param device
     *            device to be used for execution
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1026")
    public void testVerifyAmazonS3Upload(Dut device) {

	// stores the test result
	boolean result = false;
	// stores the test case id
	String testId = "TC-RDKB-SYSTEM-026";
	// stores the error message
	String errorMessage = null;
	// stores the command response
	String response = null;
	// stores the test step number
	String step = "s1";
	// stores the Result values response
	ResultValues resValues = new ResultValues();
	// stores the message list to be passed as parameter
	List<String> messageList = new ArrayList<String>();
	// stores the start time for poll duration
	long startTime = 0;

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-1026");
	LOGGER.info("TEST DESCRIPTION: verify minidump upload to S3/crash portal");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Obtain Pid of CcspPandMSsp process.");
	LOGGER.info("2. Create a file /nvram/coredump.properties and override the S3_AMAZON_SIGNING_URL");
	LOGGER.info("3. Kill CcspPandMSsp process and verify it has started again.");
	LOGGER.info("4. Poll and check core_log.txt for upload failed and retry log messages ");
	LOGGER.info(
		"5. Poll and check for fail over mechanism log message not present in core_log.txt. If Fail over mechanism fails,need to check for that log message is not present");
	LOGGER.info("6. Verify minidump is not uploaded to new Failover servers.");
	LOGGER.info("7. Check if minidump folder has been cleaned up and the log messages present");
	LOGGER.info("8. Remove /nvram/coredump.properties file");
	LOGGER.info("9. Obtain Pid of CcspPandMSsp process.");
	LOGGER.info("10. Kill CcspPandMSsp process and verify it has started again.");
	LOGGER.info("11. Poll and check for S3 Amazon upload success log message in core_log.txt");
	LOGGER.info("12. Check if minidump folder has been cleaned up and the log messages present");
	LOGGER.info("#######################################################################################");
	try {

	    /*
	     * Step 1: Obtain Pid of CcspPandMSsp process.
	     */
	    errorMessage = "Unable to obtain the pid for CcspPandMSsp";
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 1:DESCRIPTION: Obtain Pid of CcspPandMSsp process.");
	    LOGGER.info("STEP 1:ACTION: Execute command:pidof CcspPandMSsp");
	    LOGGER.info("STEP 1:EXPECTED - Pid of CcspPandMSsp process is obtained");
	    LOGGER.info("*****************************************************************************************");
	    response = CommonMethods.getPidOfProcess(device, tapEnv, StbProcess.PANDM.getProcessName());
	    result = CommonMethods.isNotNull(response);
	    if (result) {
		LOGGER.info("STEP 1: ACTUAL :Pid of CcspPandMSsp process is obtained");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 2: Create a file /nvram/coredump.properties and override the S3_AMAZON_SIGNING_URL
	     */
	    step = "s2";
	    errorMessage = "Unable to create file /nvram/coredump.properties with incorrect url";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info(
		    "STEP 2:DESCRIPTION:  Create a file /nvram/coredump.properties and override the S3_AMAZON_SIGNING_URL.");
	    LOGGER.info(
		    "STEP 2:ACTION: Execute command: echo 'S3_AMAZON_SIGNING_URL=http://test' > /nvram/coredump.properties");
	    LOGGER.info("STEP 2:EXPECTED - File is created with incorrect S3 Amazon Signing Url");
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_AMAZON_URL_OVERRIDE);
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTestConstants.PROP_KEY_AMAZON_URL,
		    BroadBandCommandConstants.FILE_NVRAM_COREDUMP_PROPERTIES);
	    result = CommonMethods.isNotNull(response);
	    if (result) {
		LOGGER.info("STEP 2: ACTUAL :File is created with incorrect S3 Amazon Signing Url");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 3: Kill CcspPandMSsp process and verify it has started again.
	     */
	    step = "s3";
	    errorMessage = "Unable to kill and verify CcspPandMSsp restarted with new pid";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 3:DESCRIPTION: Kill CcspPandMSsp and verify it has started again.");
	    LOGGER.info("STEP 3:ACTION: Execute command:kill -11 <pid_of_ccsp_process>.");
	    LOGGER.info("STEP 3:EXPECTED - CcspPandMSsp has been killed and started with new pid");
	    LOGGER.info("*****************************************************************************************");
	    result = CommonMethods.restartProcess(device, tapEnv, ProcessRestartOption.KILL_11,
		    StbProcess.PANDM.getProcessName());
	    if (result) {
		LOGGER.info("STEP 3: ACTUAL :CcspPandMSsp has been killed and started with new pid");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 4: Poll and check core_log.txt for upload failed and retry log messages
	     */
	    step = "s4";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 4:DESCRIPTION: Poll and check core_log.txt for upload failed and retry log messages.");
	    LOGGER.info(
		    "STEP 4:ACTION: Execute commands:grep -i 'S3 Amazon Upload Failed' /rdklogs/logs/core_log.txt,grep -i 'Retry' /rdklogs/logs/core_log.txt");
	    LOGGER.info("STEP 4:EXPECTED - S3 Upload failed and Retry log messages present");
	    LOGGER.info("*****************************************************************************************");
	    messageList.clear();
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_AMAZON_UPLOAD_FAILED);
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_AMAZON_UPLOAD_RETRY);
	    startTime = System.currentTimeMillis();
	    do {
		resValues = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			CrashConstants.LOG_FILE_FOR_CRASHES_RDKB, true);
		result = resValues.isResult();
		errorMessage = resValues.getMessage();
		if (result) {
		    break;
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (result) {
		LOGGER.info("STEP 4: ACTUAL :S3 Upload failed and Retry log messages present");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 5: Poll and check for fail over mechanism log message in core_log.txt. If Fail over mechanism fails,
	     * need to check for that log message.
	     */
	    step = "s5";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info(
		    "STEP 5:DESCRIPTION: Poll and check for fail over mechanism log message in core_log.txt. If Fail over mechanism fails, need to check for that log message is not present");
	    LOGGER.info(
		    "STEP 5:ACTION: Execute command: to search for logs-Fail Over Mechanism: CURL minidump to crashportal,Fail Over Mechanism for minidump : Failed..! in /rdklogs/logs/core_log.txt ");
	    LOGGER.info(
		    "STEP 5:EXPECTED - Fail over mechansism log messages should not present in /rdklogs/logs/core_log.txt");
	    LOGGER.info("*****************************************************************************************");
	    messageList.clear();
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_FAIL_OVER_UPLOAD);
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_CRASH_PORTAL_SUCCESS_UPLOAD);
	    startTime = System.currentTimeMillis();
	    errorMessage = "Fail over mechanism log messages is present in  /rdklogs/logs/core_log.txt";
	    do {
		resValues = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			CrashConstants.LOG_FILE_FOR_CRASHES_RDKB, true);
		result = !resValues.isResult();
		if (!result) {
		    messageList.remove(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_CRASH_PORTAL_SUCCESS_UPLOAD);
		    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_FAIL_OVER_FAILED);
		    resValues = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			    CrashConstants.LOG_FILE_FOR_CRASHES_RDKB, true);
		    result = !resValues.isResult();
		}
		if (result) {
		    break;
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (result) {
		LOGGER.info(
			"STEP 5: ACTUAL :Fail over mechansism log messages is not present in /rdklogs/logs/core_log.txt");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 6: Verify minidump is uploaded to new Failover servers by checking for upload log string in
	     * core_log.txt file
	     */
	    step = "s6";
	    errorMessage = "Upload string log message present";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 6:DESCRIPTION: Verify minidump is not uploaded to new Failover servers.");
	    LOGGER.info("STEP 6:ACTION: Execute command:grep -i 'Upload string' /rdklogs/logs/core_log.txt.");
	    LOGGER.info("STEP 6:EXPECTED - The upload string in core_log should not contain new failover upload url");
	    LOGGER.info("*****************************************************************************************");
	    startTime = System.currentTimeMillis();
	    errorMessage = BroadBandCommonUtils
		    .concatStringUsingStringBuffer("Upload string contains new crash failover upload url");
	    do {
		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_UPLOAD_STRING, CrashConstants.LOG_FILE_FOR_CRASHES_RDKB);
		result = CommonMethods.isNull(response);
		if (!result) {
		    errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
			    "Upload string contains new crash failover upload url: ",
			    BroadBandTestConstants.RDKB_CRASH_FAILOVER_UPLOAD_URL);
		    result = !CommonMethods.patternMatcher(response,
			    BroadBandTestConstants.RDKB_CRASH_FAILOVER_UPLOAD_URL);
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TWO_MINUTE_IN_MILLIS && !result
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
	    if (result) {
		LOGGER.info("STEP 6: ACTUAL : The upload string in core_log does not contains new failover upload url");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 7: Check if minidump folder has been cleaned up and the log messages present.
	     */
	    step = "s7";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info(
		    "STEP 7:DESCRIPTION: Check if minidump folder has been cleaned up and the log messages present.");
	    LOGGER.info(
		    "STEP 7:ACTION: Execute commands:grep -i 'Cleanup minidump directory /minidumps' /rdklogs/logs/core_log.txt.");
	    LOGGER.info("STEP 7:EXPECTED - The minidump folder is empty and log messages are present");
	    LOGGER.info("*****************************************************************************************");
	    messageList.clear();
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_CLEANUP_DIRECTORY);
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_WORKING_DIR_EMPTY);
	    startTime = System.currentTimeMillis();
	    do {
		resValues = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			CrashConstants.LOG_FILE_FOR_CRASHES_RDKB, true);
		errorMessage = resValues.getMessage();
		result = resValues.isResult();
		if (result) {
		    break;
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (result) {
		LOGGER.info("STEP 7: ACTUAL : The minidump folder is empty and log messages are present");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 8: Remove /nvram/coredump.properties file
	     */
	    step = "s8";
	    errorMessage = "File was not removed successfully";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 8:DESCRIPTION: Remove /nvram/coredump.properties file.");
	    LOGGER.info("STEP 8:ACTION: Execute command:rm /nvram/coredump.properties.");
	    LOGGER.info("STEP 8:EXPECTED - File /nvram/coredump.properties does not exist");
	    LOGGER.info("*****************************************************************************************");

	    result = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommandConstants.FILE_NVRAM_COREDUMP_PROPERTIES);
	    if (result) {
		LOGGER.info("STEP 8: ACTUAL : File /nvram/coredump.properties does not exist");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 9: Obtain Pid of CcspPandMSsp process.
	     */
	    step = "s9";
	    errorMessage = "Unable to obtain the pid for CcspPandMSsp";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 9:DESCRIPTION: Obtain Pid of CcspPandMSsp process.");
	    LOGGER.info("STEP 9:ACTION: Execute command:pidof CcspPandMSsp,echo > /rdklogs/logs/core_log.txt.");
	    LOGGER.info("STEP 9:EXPECTED - Pid of CcspPandMSsp process is obtained");
	    LOGGER.info("*****************************************************************************************");

	    // Clearing core_log file to avoid false pass since we're checking
	    // same messages again
	    LOGGER.info("Clearing core_log file:");
	    CommonUtils.clearLogFile(tapEnv, device, CrashConstants.LOG_FILE_FOR_CRASHES_RDKB);
	    response = CommonMethods.getPidOfProcess(device, tapEnv, StbProcess.PANDM.getProcessName());
	    result = CommonMethods.isNotNull(response);
	    // Restarting PAM process for devices without auto restart
	    if (!result) {
		LOGGER.info("Restarting PandM process:");
		tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_START_PANDM_PROCESS);
		response = CommonMethods.getPidOfProcess(device, tapEnv, StbProcess.PANDM.getProcessName());
		result = CommonMethods.isNotNull(response);
	    }
	    if (result) {
		LOGGER.info("STEP 9: ACTUAL : Pid of CcspPandMSsp process is obtained");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 10: Kill CcspPandMSsp process and verify it has started again.
	     */
	    step = "s10";
	    errorMessage = "Unable to kill and verify CcspPandMSsp restarted with new pid";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 10:DESCRIPTION: Kill CcspPandMSsp process and verify it has started again.");
	    LOGGER.info("STEP 10:ACTION: Execute command:kill -11 <pid_of_ccsp_process>");
	    LOGGER.info("STEP 10:EXPECTED - CcspPandMSsp has been killed and started with new pid");
	    LOGGER.info("*****************************************************************************************");
	    result = CommonMethods.restartProcess(device, tapEnv, ProcessRestartOption.KILL_11,
		    StbProcess.PANDM.getProcessName());
	    if (result) {
		LOGGER.info("STEP 10: ACTUAL : CcspPandMSsp has been killed and started with new pid");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 11: Poll and check for S3 Amazon upload success log message in core_log.txt
	     */
	    step = "s11";
	    errorMessage = "Unable to find S3 Amazon upload success log message";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info(
		    "STEP 11:DESCRIPTION: Poll and check for S3 Amazon upload success log message in core_log.txt.");
	    LOGGER.info(
		    "STEP 11:ACTION: Execute command:grep -i 'S3 minidump Upload is successful with TLS1.2' /rdklogs/logs/core_log.txt.");
	    LOGGER.info("STEP 11:EXPECTED -S3 Amazon upload success log message is present");
	    LOGGER.info("*****************************************************************************************");

	    startTime = System.currentTimeMillis();
	    do {
		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_AMAZON_UPLOAD_SUCCESS,
			CrashConstants.LOG_FILE_FOR_CRASHES_RDKB);
		result = CommonMethods.isNotNull(response);
		if (result) {
		    break;
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (result) {
		LOGGER.info("STEP 11: ACTUAL : S3 Amazon upload success log message is present");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 12: Check if minidump folder has been cleaned up and the log messages present.
	     */
	    step = "s12";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info(
		    "STEP 12:DESCRIPTION: Check if minidump folder has been cleaned up and the log messages present.");
	    LOGGER.info(
		    "STEP 12:ACTION: Execute commands:grep -i 'Cleanup minidump directory /minidumps' /rdklogs/logs/core_log.txt.");
	    LOGGER.info("STEP 12:EXPECTED -The minidump folder is empty and log messages are present");
	    LOGGER.info("*****************************************************************************************");
	    messageList.clear();
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_CLEANUP_DIRECTORY);
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_WORKING_DIR_EMPTY);
	    startTime = System.currentTimeMillis();
	    do {
		resValues = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			CrashConstants.LOG_FILE_FOR_CRASHES_RDKB, true);
		errorMessage = resValues.getMessage();
		result = resValues.isResult();
		if (result) {
		    break;
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (result) {
		LOGGER.info("STEP 12: ACTUAL : The minidump folder is empty and log messages are present");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured while validating minidump upload to S3/crash portal");
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, result, errorMessage, true);
	}
    }
}

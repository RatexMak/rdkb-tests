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

package com.automatics.rdkb.tests.telemetry;

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTelemetryConstants;
import com.automatics.rdkb.constants.BroadBandTelemetryConstants.BroadBandTelemetryProfile;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetryUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandTelemetryFrequencyPollingTests extends AutomaticsTestBase {

    /** Constant holds the test step number **/
    private static boolean initialDcaFalg = false;

    /** Constant holds the previous DCA counter value **/
    private static int previousDcaCounterValue = 0;

    /** Constant holds the test step number **/
    private static String stepNumber = "s1";

    /** Constant holds the error message **/
    private static String errorMessage = null;
    
    /**
     * Telemetry upload failure recovery
     * 
     * <p>
     * Steps:
     * </p>
     * <ol>
     * <li>STEP 1: Copy dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL.</li>
     * <li>STEP 2: Reboot the device and wait for IP acquisition.</li>
     * <li>STEP 3: Validate modified url in dcmscript.log file</li>
     * <li>STEP 4: Verify whether telemetry is scheduled to 5 min in atom console</li>
     * <li>STEP 5: Verify whether DCMresponse.txt file is available in the device under /nvram</li>
     * <li>STEP 6: Verify the settings are reflecting in DCMresponse.txt</li>
     * <li>Step 7: Add mock log for logging expected string for telemetry</li>
     * <li>Step 8: Verify the telemetry logs based on dcaCounter value</li>
     * <li>Step 9: Verify CPU usage Repeat</li>
     * <li>Step 10 - Step 30, Repeat step 7 to 10 for all telemetry markers, Note - Telemetry markers upload depends on
     * the</li>
     * <li>dca counter value. so it varies for each execution, but all markers will be validated.</li>
     * </ol>
     * 
     * @author Praveenkumar Paneerselvam
     * @refactor Rakesh C N
     * 
     * @param device
     *            instance of {@link Dut}
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	     TestGroup.WEBPA })
    @TestDetails(testUID = "TC-RDKB-TMTRY_UPLD_RCVRY-1015")
    public void testToVerifyThrottleTelemetrySamples(Dut device) {
	// boolean variable to store the status
	boolean status = false;
	// Test case id
	String testId = "TC-RDKB-TMTRY_UPLD_RCVRY-015";
	// Test step number
	stepNumber = "s1";
	// error message
	errorMessage = null;
	initialDcaFalg = false;
	previousDcaCounterValue = 0;
	String expectedPattern = null;
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-TMTRY_UPLD_RCVRY-1015");
	    LOGGER.info("TEST DESCRIPTION: Verify Telemetry Polling frequency");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("STEP 1: Copy dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL.");
	    LOGGER.info("STEP 2: Reboot the device and wait for IP acquisition.");
	    LOGGER.info("STEP 3: Validate modified url in dcmscript.log file");
	    LOGGER.info("STEP 4: Verify whether telemetry is scheduled to 5 min in atom console");
	    LOGGER.info("STEP 5: Verify whether DCMresponse.txt file is available in the device under /nvram");
	    LOGGER.info("STEP 6: Verify the settings are reflecting in DCMresponse.txt");
	    LOGGER.info("Step 7: Add mock log for logging expected string for telemetry");
	    LOGGER.info("Step 8: Verify the telemetry logs based on dcaCounter value");
	    LOGGER.info("Step 9: Verify CPU usage Repeat");
	    LOGGER.info(
		    "Step 10 - Step 30, Repeat step 7 to 10 for all telemetry markers, Note - Telemetry markers upload depends on the dca counter value. so it varies for each execution, but all markers will be validated.");
	    // Posting the profile along with the existing profile.
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : TELEMETRY 2 STATUS VALIDATION
	     */
	    BroadBandPreConditionUtils.executePreConditionToVerifyTelemetryStatus(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    /**
	     * PRE-CONDITION 2 : Upload telemetry profile to Xconf
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : Upload telemetry profile to Xconf ");
	    LOGGER.info("PRE-CONDITION 2 : ACTION : post telemetry data to proxy dcm server ");
	    LOGGER.info("PRE-CONDITION 2 : EXPECTED : Post request to Xconf should respond with HTTP 200 code ");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to Post telemetry data to XConf";
	    status = (BroadBandTelemetryUtils.postDataToProxyDcmServer(device, tapEnv, false,
		    true) == BroadBandTestConstants.CONSTANT_200);
	    if (status) {
		LOGGER.info("PRE-CONDITION 2 : ACTUAL : Uploaded telemetry profile to Xconf successfully");
	    } else {
		LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * Step 1 : Copy dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL.
	     */
	    status = false;

	    LOGGER.info("*************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Copy dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL");
	    LOGGER.info(
		    "STEP 1: ACTION : Copy the dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL");
	    LOGGER.info("STEP 1: EXPECTED : The file should be copied and updated");
	    LOGGER.info("*************************************************************************");
	    int validationStatus = BroadBandTelemetryUtils.copyAndUpdateDcmProperties(device, tapEnv);
	    // updating the test case based on the return value from the helper
	    // method
	    if (BroadBandTestConstants.CONSTANT_0 == validationStatus) {
		errorMessage = "dcm.properties is not present, missing, in /etc/ folder";
	    } else if (BroadBandTestConstants.CONSTANT_2 == validationStatus) {
		errorMessage = "Successfuly copied /etc/dcm.properties to /nvram folder, But Failed to update Xconf url!!!!";
	    } else {
		status = true;
	    }
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Successfully copied dcm.properties file to /nvram and verified the DCM_LOG_SERVER_URL");
	    } else {
		LOGGER.error("STEP 1: ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("*************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

	    /**
	     * Step 2 : Reboot the device and wait for IP acquisition.
	     */
	    stepNumber = "s2";
	    status = false;
	    LOGGER.info("*************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Reboot the device and wait for IP acquisition");
	    LOGGER.info("STEP 2: ACTION : Execute Command - /sbin/reboot");
	    LOGGER.info("STEP 2: EXPECTED : Device should be rebooted");
	    LOGGER.info("*************************************************************************");
	    status = BroadBandCommonUtils.rebootAndWaitForStbAccessible(device, tapEnv);
	    LOGGER.info("Device reboot status is - " + status);
	    errorMessage = "Failed to reboot the device. Checked for 10 min to access the device after reboot.";
	    if (!status) {
		errorMessage = "Webpa Process is not up and running";
		status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully reboot the STB and able to access the device");
	    } else {
		LOGGER.error("STEP 2: ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("*************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

	    /**
	     * Step 3 : Validate modified url in dcmscript.log file.
	     */
	    stepNumber = "s3";
	    status = false;
	    LOGGER.info("*************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Validate modified url in dcmscript.log file");
	    LOGGER.info("STEP 3: ACTION : Check for modified url in dcmscript.log file");
	    LOGGER.info("STEP 3: EXPECTED : File should have modified xconf url.");
	    LOGGER.info("*************************************************************************");
	    errorMessage = "Modified URL is not present in dcmscript.log file";
	    try {
		BroadBandTelemetryUtils.verifyXconfDcmConfigurationUrlAndDownloadStatusFromDcmScriptLogs(device, tapEnv,
			BroadBandTestConstants.PROP_KEY_PROXY_XCONF_URL);
		status = true;
	    } catch (TestException exception) {
		errorMessage += exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Modified URL is present in dcmscript.log file");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    // Creating the backup file for live debug and it will be deleted d
	    List<String> mustHaveLogFileList = new ArrayList<String>();
	    String dcmscript = BroadBandCommandConstants.LOG_FILE_DCM_SCRIPT
		    .replace(BroadBandCommandConstants.DIRECTORY_LOGS, BroadBandTestConstants.EMPTY_STRING);
	    mustHaveLogFileList.add(dcmscript);
	    BroadBandCommonUtils.verifyRdkLogAlbltyAndTailLogToGivenPathAndConsole(device, tapEnv, mustHaveLogFileList,
		    CommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.SYMBOL_PLUS,
			    BroadBandTestConstants.STRING_VALUE_ONE),
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, CommonMethods.isAtomSyncAvailable(device, tapEnv)
			    ? BroadBandTestConstants.STRING_ATOM_CONSOLE : BroadBandTestConstants.ARM,
		    BroadBandTestConstants.NVRAM_PATH);

	    /**
	     * Step 4 : Verify whether telemetry is scheduled to 5 min in atom console.
	     */
	    stepNumber = "s4";
	    status = false;
	    LOGGER.info("*************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify whether telemetry is scheduled to 5 min in atom console");
	    LOGGER.info("STEP 4: ACTION : Check for dca_utility.sh in cat /tmp/cron/root in atom console.");
	    LOGGER.info("STEP 4: EXPECTED : Cron job should have waiting time of 5 min.");
	    LOGGER.info("*************************************************************************");
	    errorMessage = "Failed to verify telemetry";
	    long startTime = System.currentTimeMillis();
	    do {
		try {
		    BroadBandTelemetryUtils.verifyDcmCronJobsConfiguredBasedOnDcmSettings(device, tapEnv,
			    BroadBandTelemetryUtils.SCHEDULE_CRON_JOB_TIME_FOR_TELEMETRY);
		    status = true;
		} catch (TestException exception) {
		    errorMessage = exception.getMessage();
		}
	    } while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Telemetry is scheduled to 5 min as expected");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /**
	     * s5. Verify whether DCM Settings file is present in the device
	     */
	    stepNumber = "s5";
	    status = false;
	    LOGGER.info("*************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify whether DCMresponse.txt file is available in the device under /nvram");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the command ls /nvram and verify whether the  DCMresponse.txt file is available");
	    LOGGER.info("STEP 5: EXPECTED : The file should be available");
	    LOGGER.info("*************************************************************************");
	    errorMessage = "The DCMSettings.conf file is not available under /tmp";
	    startTime = System.currentTimeMillis();
	    do {
		LOGGER.info("Waiting for device to be accessed");
		status = CommonUtils.isFileExists(device, tapEnv, BroadBandTestConstants.FILE_NVRAM_DCMRESPONSE);
	    } while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : DCMresponse.txt file is available in the device under /nvram");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /**
	     * s6. Verify the settings are reflecting in DCMSettings.conf
	     */
	    stepNumber = "s6";
	    status = false;
	    LOGGER.info("*************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify the settings are reflecting in DCMresponse.txt");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the command cat /tmp/DCMresponse.txt and verify whether all the required profiles are available");
	    LOGGER.info(
		    "STEP 6: EXPECTED : /nvram/DCMresponse.txt  should contain all the Telemetry rules in a JSON Format");
	    LOGGER.info("*************************************************************************");
	    errorMessage = "The required profiles are not available in DCMresponse.txt!!!";
	    status = verifyTelemtryThrottleProfilesInDcmResponseFile(device, tapEnv);
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Settings are reflected in DCMresponse.txt");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

	    startTime = System.currentTimeMillis();
	    /**
	     * s7 -s30. Common Step : To add logging in expected log file and validating in dcmscript.log and verifying
	     * the cpu usage percentage.
	     */
	    int stepNo = BroadBandTestConstants.CONSTANT_0;
	    // Validation in progress
	    String mockLogs = BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_1.getTelemetryProfileContent(),
		    BroadBandTestConstants.NEW_LINE_WITH_ESCAPE_CHARACTER,
		    BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_2.getTelemetryProfileContent(),
		    BroadBandTestConstants.NEW_LINE_WITH_ESCAPE_CHARACTER,
		    BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_3.getTelemetryProfileContent(),
		    BroadBandTestConstants.NEW_LINE_WITH_ESCAPE_CHARACTER,
		    BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_4.getTelemetryProfileContent(),
		    BroadBandTestConstants.NEW_LINE_WITH_ESCAPE_CHARACTER,
		    BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_5.getTelemetryProfileContent(),
		    BroadBandTestConstants.NEW_LINE_WITH_ESCAPE_CHARACTER,
		    BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_6.getTelemetryProfileContent(),
		    BroadBandTestConstants.NEW_LINE_WITH_ESCAPE_CHARACTER,
		    BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_7.getTelemetryProfileContent(),
		    BroadBandTestConstants.NEW_LINE_WITH_ESCAPE_CHARACTER,
		    BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_8.getTelemetryProfileContent());

	    for (stepNo = BroadBandTestConstants.CONSTANT_7; stepNo < 30; stepNo = stepNo
		    + BroadBandTestConstants.CONSTANT_3) {
		if (stepNo == BroadBandTestConstants.CONSTANT_7 || stepNo == BroadBandTestConstants.CONSTANT_28) {
		    expectedPattern = BroadBandTelemetryConstants.TELEMETRY_PATTERN_FOR_DCA_COUNT_1;
		}
		if (stepNo == BroadBandTestConstants.CONSTANT_10) {
		    expectedPattern = BroadBandTelemetryConstants.TELEMETRY_PATTERN_FOR_DCA_COUNT_2;
		}
		if (stepNo == BroadBandTestConstants.CONSTANT_13) {
		    expectedPattern = BroadBandTelemetryConstants.TELEMETRY_PATTERN_FOR_DCA_COUNT_3;
		}
		if (stepNo == BroadBandTestConstants.CONSTANT_16) {
		    expectedPattern = BroadBandTelemetryConstants.TELEMETRY_PATTERN_FOR_DCA_COUNT_4;
		}

		if (stepNo == BroadBandTestConstants.CONSTANT_19) {
		    expectedPattern = BroadBandTelemetryConstants.TELEMETRY_PATTERN_FOR_DCA_COUNT_5;
		}
		if (stepNo == BroadBandTestConstants.CONSTANT_22) {
		    expectedPattern = BroadBandTelemetryConstants.TELEMETRY_PATTERN_FOR_DCA_COUNT_6;
		}
		if (stepNo == BroadBandTestConstants.CONSTANT_25) {
		    expectedPattern = BroadBandTelemetryConstants.TELEMETRY_PATTERN_FOR_DCA_COUNT_7;
		}
		// Common method
		executeAndVerifyTelemetrySteps(device, stepNo, testId, expectedPattern, mockLogs);
	    }
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, errorMessage, true);
	} finally {
	    boolean isFileExist = false;
	    try {
		isFileExist = CommonUtils.isFileExists(device, tapEnv,
			BroadBandTestConstants.DCM_PROPERTIES_FILE_NVRAM_FOLDER);
	    } catch (Exception exception) {
		errorMessage = exception.getMessage();
	    }
	    if (isFileExist) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		// Post Condition to revert the telemetry changes. Delete the dcm.properties in nvram file.
		BroadBandPostConditionUtils.executePostConditionToRemoveBackUpFile(device, tapEnv,
			BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.DCM_PROPERTIES_FILE_NVRAM_FOLDER);
		if (!CommonUtils.isFileExists(device, tapEnv,
			BroadBandTestConstants.DCM_PROPERTIES_FILE_NVRAM_FOLDER)) {
		    BroadBandPostConditionUtils.postConditionToRebootAndWaitForIpAccusition(device, tapEnv,
			    BroadBandTestConstants.CONSTANT_2);
		}

		LOGGER.info("##########################################################################");
		LOGGER.info("POST-CONDITION 3 : DESCRIPTION : DELETE TEMPORARY FILE FROM NVRAM");
		LOGGER.info("POST-CONDITION 3 : ACTION : EXECUTE COMMAND rm <filename>");
		LOGGER.info("POST-CONDITION 3 : EXPECTED : FILE SHOULD BE REMOVED SUCCESSFULLY");
		LOGGER.info("##########################################################################");
		List<String> logFileList = new ArrayList<>();
		logFileList.add(
			(CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.STRING_AUTOMATION_BACK_UP,
				BroadBandCommandConstants.LOG_FILE_DCM_SCRIPT.replace(
					BroadBandCommandConstants.DIRECTORY_LOGS,
					BroadBandTestConstants.EMPTY_STRING))));
		for (String logFile : logFileList) {
		    boolean logFileExists = CommonUtils.isFileExists(device, tapEnv,
			    CommonMethods.concatStringUsingStringBuffer(
				    BroadBandCommandConstants.NAVIGATE_GIVEN_FILE_IN_NVRAM
					    .replace(BroadBandTestConstants.STRING_REPLACE, logFile)));
		    if (logFileExists) {
			status = CommonUtils.deleteFile(device, tapEnv,
				CommonMethods.concatStringUsingStringBuffer(
					BroadBandCommandConstants.NAVIGATE_GIVEN_FILE_IN_NVRAM
						.replace(BroadBandTestConstants.STRING_REPLACE, logFile)));
		    }
		}
		if (status) {
		    LOGGER.info("POST-CONDITION 3: ACTUAL : Deleted temporary created file");
		} else {
		    LOGGER.error("POST-CONDITION 3: ACTUAL :Unable to delete temporary file");
		}

		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-TMTRY_UPLD_RCVRY-1015");
    }
    
    /**
     * 
     * Helper method to verify whether all the required profiles for testing telemetry throttle is available.
     * 
     * @param device
     *            {@link Dut}
     * @param tapApi
     *            {@link AutomaticsTapApi}
     * @return true if profiles are available, else false
     * @author Praveenkumar Paneerselvam
     * @refactor Rakesh C N
     */
    private static boolean verifyTelemtryThrottleProfilesInDcmResponseFile(Dut device, AutomaticsTapApi tapApi) {
	LOGGER.debug("STARTING METHOD: verifyTelemtryThrottleProfilesInDcmSettingsConf");
	boolean status = false;
	/*
	 * Getting the contents of DCMResponse.txt file and verifying whether all the required profiles for testing
	 * telemetry throttle is available.
	 * 
	 */
	String command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_GREP_OPTION_E,
		BroadBandTestConstants.DOUBLE_QUOTE,
		BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_1.getTelemetryProfileContent(),
		BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandTestConstants.FILE_NVRAM_DCMRESPONSE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandCommandConstants.CMD_GREP_OPTION_E, BroadBandTestConstants.DOUBLE_QUOTE,
		BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_2.getTelemetryProfileContent(),
		BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandCommandConstants.CMD_GREP_OPTION_E, BroadBandTestConstants.DOUBLE_QUOTE,
		BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_3.getTelemetryProfileContent(),
		BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandCommandConstants.CMD_GREP_OPTION_E, BroadBandTestConstants.DOUBLE_QUOTE,
		BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_4.getTelemetryProfileContent(),
		BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandCommandConstants.CMD_GREP_OPTION_E, BroadBandTestConstants.DOUBLE_QUOTE,
		BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_5.getTelemetryProfileContent(),
		BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandCommandConstants.CMD_GREP_OPTION_E, BroadBandTestConstants.DOUBLE_QUOTE,
		BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_6.getTelemetryProfileContent(),
		BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandCommandConstants.CMD_GREP_OPTION_E, BroadBandTestConstants.DOUBLE_QUOTE,
		BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_7.getTelemetryProfileContent(),
		BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		BroadBandCommandConstants.CMD_GREP_OPTION_E, BroadBandTestConstants.DOUBLE_QUOTE,
		BroadBandTelemetryProfile.TELEMETRY_TEST_PROFILE_8.getTelemetryProfileContent(),
		BroadBandTestConstants.DOUBLE_QUOTE);

	// verifying whether all the profiles are available in DCMSettings.conf
	LOGGER.info("Command to be executed is - " + command);
	if (CommonMethods.isNotNull(tapEnv.executeCommandUsingSsh(device, command))) {
	    status = true;
	} else {
	    throw new TestException("DCMresponse.txt file response is null. No response in DCMresponse.txt file");
	}
	LOGGER.info("Is telemetry throttle profile present in DCM response file - " + status);
	LOGGER.debug("ENDING METHOD: verifyTelemtryThrottleProfilesInDcmSettingsConf");
	return status;
    }
    
    /**
     * Method to execute common steps 1. Add mock logs in expected log files for telemetry event trigger. 2.Check for
     * telemetry event in dcmscript.log 3. Validate cpu usage
     * 
     * @param device
     *            Dut instance
     * @param stepNo
     *            starting step number
     * @param testId
     *            test id
     * @param expectedPattern
     *            Expected pattern to validate
     * @param mocklogs
     *            mock log
     *            
     * @author Praveenkumar Paneerselvam
     * @refactor Rakesh C N
     */
    private void executeAndVerifyTelemetrySteps(Dut device, int stepNo, String testId, String expectedPattern,
	    String mockLogs) {
	long startTime = System.currentTimeMillis();
	int presentDcaCounterValue = 0;
	boolean status = false;
	// Validating whether dca file is created with valid value.
	do {
	    presentDcaCounterValue = BroadBandTelemetryUtils.getDcaCounterValueForTelemetry(device, tapEnv);
	    // if DCA counter value is 0 , It will be only one time
	    if (!initialDcaFalg && presentDcaCounterValue == 0) {
		status = true;
		initialDcaFalg = status;
	    }
	    // if DCA counter value is -1 , file not available or unable to get the count
	    if (presentDcaCounterValue == -1) {
		status = false;
	    }
	    // if DCA counter value is greater than 0 , It will from step 10
	    if (presentDcaCounterValue > 0) {
		previousDcaCounterValue = presentDcaCounterValue;
		status = true;
	    }
	} while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS
		&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS));
	LOGGER.info("DCA Counter value before adding mock logs : " + previousDcaCounterValue);

	/**
	 * Common Step : Add mock logs for logging expected string for telemetry.
	 */
	stepNumber = "s" + stepNo;
	status = false;
	LOGGER.info("*************************************************************************");
	LOGGER.info("STEP " + stepNo + ": DESCRIPTION : Add mock logs for logging expected string for telemetry");
	LOGGER.info("STEP " + stepNo + ": ACTION : Execute commands to add mock logs in LM.txt.0");
	LOGGER.info("STEP " + stepNo + ": EXPECTED : The commands should be executed without any issue");
	LOGGER.info("*************************************************************************");
	errorMessage = "Failed to execute the commands for adding mock step";
	status = BroadBandTelemetryUtils.executeCommandsForMockingLogsForTelemetryThrottle(device, tapEnv, mockLogs,
		BroadBandCommandConstants.LOG_FILE_LM);
	if (status) {
	    LOGGER.info("STEP " + stepNo + ": ACTUAL : Mock logs added in the log file successfully");
	} else {
	    LOGGER.error("STEP " + stepNo + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("*************************************************************************");
	tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	// Validating whether next polling has happened after adding the mock values.
	startTime = System.currentTimeMillis();
	status = false;
	do {
	    presentDcaCounterValue = BroadBandTelemetryUtils.getDcaCounterValueForTelemetry(device, tapEnv);
	    LOGGER.info("DCA Counter old value : " + previousDcaCounterValue);
	    LOGGER.info("DCA Counter new value : " + presentDcaCounterValue);
	    if (previousDcaCounterValue == presentDcaCounterValue) {
		status = false;
	    }
	    if (presentDcaCounterValue > previousDcaCounterValue) {
		previousDcaCounterValue = presentDcaCounterValue;
		status = true;
	    }
	    LOGGER.info("Waiting for next telemetry polling to start. newDcaCounter newDcaCounter >dcaCounter status:"
		    + (presentDcaCounterValue > previousDcaCounterValue));
	} while (!status
		&& ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS)
		&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS));

	/**
	 * Common Steps : Verify the telemetry logs for first execution
	 */
	stepNumber = "s" + (++stepNo);
	status = false;
	LOGGER.info("*************************************************************************");
	LOGGER.info("STEP " + stepNo + ": DESCRIPTION : Verify the telemetry logs for execution");
	LOGGER.info("STEP " + stepNo
		+ ": ACTION : Execute commands cat /tmp/dcmscript.log and verify whether all the headers are getting logged.");
	LOGGER.info("STEP " + stepNo + ": EXPECTED : All the required headers must be logged under telemetry log!!!");
	LOGGER.info("*************************************************************************");
	startTime = System.currentTimeMillis();
	BroadBandResultObject executionStatus = null;
	try {
	    executionStatus = BroadBandTelemetryUtils.verifyTelemetryThrottleLogs(device, tapEnv, expectedPattern);
	    errorMessage = executionStatus.getErrorMessage();
	    status = executionStatus.isStatus();
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	}
	if (status) {
	    LOGGER.info("STEP " + stepNo + ": ACTUAL : Expected elemetry logs are available");
	} else {
	    LOGGER.error("STEP " + stepNo + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("*************************************************************************");
	tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	/**
	 * Common Step : Verify CPU usage
	 */
	stepNumber = "s" + (++stepNo);
	status = false;
	LOGGER.info("*************************************************************************");
	LOGGER.info("STEP " + stepNo + ": DESCRIPTION : Verify CPU usage ");
	LOGGER.info("STEP " + stepNo
		+ ": ACTION : Execute command and verify the CPU usage top -bn1 | grep -i Cpu(s):|CPU: | cut -d' -f1 | cut  -d':' -f2");
	LOGGER.info("STEP " + stepNo + ": EXPECTED : CPU Usage should get reported");
	LOGGER.info("*************************************************************************");
	errorMessage = "CPU Usage is not reported ";
	try {
	    Double cpuUsage = BroadBandTelemetryUtils.getCpuUsageInPercentage(device, tapEnv);
	    LOGGER.info("CPU VALUE IS - " + cpuUsage);
	    status = cpuUsage != null && cpuUsage >= BroadBandTestConstants.CONSTANT_ZERO;
	} catch (Exception exception) {
	    LOGGER.info("Exception occured while getting cpu usage. Error Message - " + exception.getMessage());
	}
	if (status) {
	    LOGGER.info("STEP " + stepNo + ": ACTUAL : CPU usage verified successsfully");
	} else {
	    LOGGER.error("STEP " + stepNo + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("*************************************************************************");
	tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);
    }
}
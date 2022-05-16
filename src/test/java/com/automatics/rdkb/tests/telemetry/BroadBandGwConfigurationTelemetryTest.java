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

import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetry2Utils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetryUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandGwConfigurationTelemetryTest extends AutomaticsTestBase {
	
    /**
     * 
     * Verify the telemetry marker & log messages for Gateway Configuration with the same SSID Name set for both 2.4 GHz
     * & 5 GHz Radios
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Pre-condition 1 : Validate if the T2 settings are already present else set the settings and reboot the device
     * </li>
     * <li>Pre-condition 2 : Set Same SSID's for both 2.4GHz and 5GHz Wifi via WEBPA.</li>
     * <li>Step 1 : Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0</li>
     * <li>Step 2 : Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console</li>
     * <li>Step 3 : Verify if 'dcmscript.log' indicates T2 is enabled</li>
     * <li>Step 4 : Verify setting the 2.4 GHz radio Private password to value: "password"</li>
     * <li>Step 5 : Verify setting the 5 GHz radio Private password to value: "password"</li>
     * <li>Step 6 : Verify the log message for same password set for both the radios.</li>
     * <li>Step 7 : Verify setting the 5 GHz radio password to value: "password_new"</li>
     * <li>Step 8 : Verify the log message for different password set for the radios.</li>
     * <li>Step 9 : Verify if T2 Markers are getting uploaded to Splunk.</li>
     * <li>Step 10 : Verify if logs are present in Splunk</li>
     * <li>Step 11 : Verify the telemetry marker for same password set for both for radios from splunk logs.</li>
     * <li>Step 12 : Verify the telemetry marker for different password set for the radios from splunk logs.</li>
     * </ol>
     * 
     * @param device
     *            {@link Instanceof Dut}
     * 
     * @author BALAJI V
     * @refactor Rakesh C N
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	     TestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-GWCONFIG-TLMTRY-5001")
    public void testGwConfigurationTelemetryMarkerSameSsidNames(Dut device) {
	// Variable Declaration Begins
	String testCaseId = "TC-RDKB-GWCONFIG-TLMTRY-501";
	boolean status = false;
	String errorMessage = null;
	int preConditionStepNum = BroadBandTestConstants.CONSTANT_0;
	boolean isTelemetryEnabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
		BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-GWCONFIG-TLMTRY-5001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the telemetry marker & log messages for Gateway Configuration with the same SSID Name set for both 2.4 GHz & 5 GHz Radios");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"Pre-condition 1 : Validate if the T2 process is up on the device else set the settings and reboot the device ");
	LOGGER.info("Pre-condition 2 : Set Same SSID's for both 2.4GHz and 5GHz Wifi via WEBPA.");
	LOGGER.info("1. Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0 ");
	LOGGER.info("2. Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console ");
	LOGGER.info("3. Verify if 'dcmscript.log' indicates T2 is enabled ");
	LOGGER.info("4. Verify setting the 2.4 GHz radio Private password to value: 'password'");
	LOGGER.info("5. Verify setting the 5 GHz radio Private password to value: 'password'");
	LOGGER.info("6. Verify the log message for same password set for both the radios");
	LOGGER.info("7. Verify setting the 5 GHz radio password to value: 'password_new'");
	LOGGER.info("8. Verify the log message for different password set for the radios");
	LOGGER.info("9. Verify if T2 Markers are getting uploaded to Splunk.");
	LOGGER.info("10. Verify if logs are present in Splunk");
	LOGGER.info("11. Verify the telemetry marker for same password set for both for radios from splunk logs.");
	LOGGER.info("12. Verify the telemetry marker for different password set for the radios from splunk logs.");
	LOGGER.info("#######################################################################################");
	try {
	    if (isTelemetryEnabled) {
		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		/**
		 * Pre-Condition 1: Executing pre-condition for telemetry version 2
		 */
		BroadBandPreConditionUtils.executePreConfigurationsForTelemetry(device, tapEnv, ++preConditionStepNum);
	    }
	    /**
	     * Pre-Condition 2: Set Same SSID's for both 2.4GHz and 5GHz Wifi via WEBPA.
	     */
	    preConditionStepNum++;
	    LOGGER.info("##################################################################################");
	    LOGGER.info("PRE-CONDITION " + preConditionStepNum
		    + " :DESCRIPTION : Set Same SSID's for both 2.4GHz and 5GHz Wifi via WEBPA.");
	    LOGGER.info("PRE-CONDITION " + preConditionStepNum
		    + " :ACTION : Execute WEBPA Command to set Same SSID for 2.4GHz and 5GHz WiFi.");
	    LOGGER.info("PRE-CONDITION " + preConditionStepNum
		    + " : EXPECTED : 2.4GHz and 5GHz with Same SSID Should be set successful via WEBPA.");
	    LOGGER.info("##################################################################################");
	    errorMessage = "UNABLE TO CONFIGURE THE PRIVATE SSID NAMES FOR 2.4GHz & 5 GHz RADIOS.";
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_DCMSCRIPT_LOG);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
		    BroadBandTestConstants.CONSTANT_0, "test-ssid-rdkb-13336")
		    && BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
			    BroadBandTestConstants.CONSTANT_0, "test-ssid-rdkb-13336");
	    if (status) {
		LOGGER.info("2.4GHz and 5GHz with Same SSID is set successfully");
	    } else {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE_CONDITION_FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

	    executeTestSteps(tapEnv, device, testCaseId);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE Setting Configuration for Telemetry or While setting the different SSID: "
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId,
		    Integer.toString(preConditionStepNum), status, errorMessage, true);
	}finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    BroadBandPostConditionUtils.executePostConditionToRemoveBackUpFile(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1,BroadBandCommandConstants.FILE_TMP_DCMSCRIPT_LOG);
	    BroadBandPostConditionUtils.executePostConditionToRemoveBackUpFile(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_2,BroadBandCommandConstants.FILE_TMP_WIFILOG);
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-GWCONFIG-TLMTRY-5001");
    }

    /**
     * 
     * Verify the telemetry marker & log messages for Gateway Configuration with the different SSID Name set for both
     * 2.4 GHz & 5 GHz Radios
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Pre-condition 1 : Validate if the T2 settings are already present else set the settings and reboot the device
     * </li>
     * <li>Pre-condition 2 : Set Different SSID's for both 2.4GHz and 5GHz Wifi via WEBPA.</li>
     * <li>Step 1 : Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0</li>
     * <li>Step 2 : Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console</li>
     * <li>Step 3 : Verify if 'dcmscript.log' indicates T2 is enabled</li>
     * <li>Step 4 : Verify setting the 2.4 GHz radio Private password to value: "password"</li>
     * <li>Step 5 : Verify setting the 5 GHz radio Private password to value: "password"</li>
     * <li>Step 6 : Verify the log message for same password set for both the radios.</li>
     * <li>Step 7 : Verify setting the 5 GHz radio password to value: "password_new"</li>
     * <li>Step 8 : Verify the log message for different password set for the radios.</li>
     * <li>Step 9 : Verify if T2 Markers are getting uploaded to Splunk.</li>
     * <li>Step 10 : Verify if logs are present in Splunk</li>
     * <li>Step 11 : Verify the telemetry marker for same password set for both radios from splunk logs.</li>
     * <li>Step 12 : Verify the telemetry marker for different password set for the radios from splunk logs.</li>
     * </ol>
     * 
     * @author BALAJI V
     * @refactor Rakesh C N
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	     TestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-GWCONFIG-TLMTRY-5002")
    public void testGwConfigurationTelemetryMarkerDifferentSsidNames(Dut device) {
	String testCaseId = "TC-RDKB-GWCONFIG-TLMTRY-502";
	boolean status = false;
	String errorMessage = null;
	int preConditionStepNum = BroadBandTestConstants.CONSTANT_1;
	// variable Declaration Ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-GWCONFIG-TLMTRY-5002");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the telemetry marker & log messages for Gateway Configuration with the different SSID Name set for both 2.4 GHz & 5 GHz Radios");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"Pre-condition 1 : Validate if the T2 process is up on the device else set the settings and reboot the device ");
	LOGGER.info("Pre-condition 2 : Set different SSID's for both 2.4GHz and 5GHz Wifi via WEBPA.");
	LOGGER.info("1. Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0 ");
	LOGGER.info("2. Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console ");
	LOGGER.info("3. Verify if 'dcmscript.log' indicates T2 is enabled ");
	LOGGER.info("4. Verify setting the 2.4 GHz radio Private password to value: 'password'");
	LOGGER.info("5. Verify setting the 5 GHz radio Private password to value: 'password'");
	LOGGER.info("6. Verify the log message for same password set for both the radios");
	LOGGER.info("7. Verify setting the 5 GHz radio password to value: 'password_new'");
	LOGGER.info("8. Verify the log message for different password set for the radios");
	LOGGER.info("9. Verify if T2 Markers are getting uploaded to Splunk.");
	LOGGER.info("10. Verify if logs are present in Splunk");
	LOGGER.info("11. Verify the telemetry marker for same password set for both for radios from splunk logs.");
	LOGGER.info("12. Verify the telemetry marker for different password set for the radios from splunk logs.");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    /**
	     * Pre-Condition 1: Executing pre-condition for telemetry version 2
	     */
	    BroadBandPreConditionUtils.executePreConfigurationsForTelemetry(device, tapEnv, preConditionStepNum);

	    /**
	     * Pre-Condition 2: Set Different SSID's for both 2.4GHz and 5GHz Wifi via WEBPA.
	     */
	    preConditionStepNum++;
	    LOGGER.info("##################################################################################");
	    LOGGER.info("PRE-CONDITION " + preConditionStepNum
		    + " :DESCRIPTION : Set Different SSID's for both 2.4GHz and 5GHz Wifi via WEBPA.");
	    LOGGER.info("PRE-CONDITION " + preConditionStepNum
		    + " :ACTION : Execute WEBPA Command to set different SSID for 2.4GHz and 5GHz WiFi.");
	    LOGGER.info("PRE-CONDITION " + preConditionStepNum
		    + " : EXPECTED : 2.4GHz and 5GHz with Different SSID Should be set successful via WEBPA.");
	    LOGGER.info("##################################################################################");
	    errorMessage = "UNABLE TO CONFIGURE THE PRIVATE SSID NAMES FOR 2.4GHz & 5 GHz RADIOS.";
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_DCMSCRIPT_LOG);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
		    BroadBandTestConstants.CONSTANT_0, "test-ssid-rdkb-13336-1")
		    && BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
			    BroadBandTestConstants.CONSTANT_0, "test-ssid-rdkb-13336-2");
	    if (status) {
		LOGGER.info("2.4GHz and 5GHz with Different SSID is set successfully");
	    } else {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE_CONDITION_FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

	    executeTestSteps(tapEnv, device, testCaseId);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE Setting Configuration for Telemetry or While setting the different SSID: "
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId,
		    Integer.toString(preConditionStepNum), status, errorMessage, true);
	}finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    BroadBandPostConditionUtils.executePostConditionToRemoveBackUpFile(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1,BroadBandCommandConstants.FILE_TMP_DCMSCRIPT_LOG);
	    BroadBandPostConditionUtils.executePostConditionToRemoveBackUpFile(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_2,BroadBandCommandConstants.FILE_TMP_WIFILOG);
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-GWCONFIG-TLMTRY-5002");
    }
    
    /**
     * Private method to perform the execution of test steps; since the same steps are required for both the test cases,
     * this method has been created.
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi}
     * @param device
     *            {@link Dut}
     * @param testCaseId
     *            String representing the test case ID.
     *            
     * @refactor Rakesh C N
     */
    private void executeTestSteps(AutomaticsTapApi tapEnv, Dut device, String testCaseId) {
	String response = null;
	int stepNumber = BroadBandTestConstants.CONSTANT_1;
	String stepNum = "S" + stepNumber;
	boolean status = false;
	String errorMessage = null;
	boolean isTelemetryEnabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
		BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	boolean logFileExists = false;
	try {
	    boolean isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	    LOGGER.info("IS ATOM SYNC AVAILABLE: " + isAtomSyncAvailable);
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    if (isTelemetryEnabled) {
		/**
		 * STEP 1 : VERIFY IF THE 'TELEMETRY2_0' PROCESS IS UP AFTER ENABLING TELEMETRY 2.0
		 */
		stepNum = "S" + stepNumber;
		errorMessage = "The process for telemetry 2 is not running on the gateway";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ " :DESCRIPTION : Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0 ");
		LOGGER.info("STEP " + stepNumber + " :ACTION : Execute the command 'ps -ww | grep telemetry2_0 ");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : The process should be up and running. ");
		LOGGER.info("**********************************************************************************");
		status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
			BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);
		if (status) {
		    LOGGER.info(
			    "STEP " + stepNum + ": ACTUAL : Telemetry 2.0 process is up and running on the gateway");
		} else {
		    LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		/**
		 * STEP 2 : VERIFY IF THE LOG FILE /rdklogs/logs/telemetry2_0.txt.0 IS PRESENT IN ARM CONSOLE
		 */
		++stepNumber;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = "The file /rdklogs/logs/telemetry2_0.txt.0 is not present on the device";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ " :DESCRIPTION : Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console ");
		LOGGER.info("STEP " + stepNumber
			+ " :ACTION : Execute the command , 'ls -lrt /rdklogs/logs/telemetry2_0.txt.0' ");
		LOGGER.info("STEP " + stepNumber
			+ " : EXPECTED : The file /rdklogs/logs/telemetry2_0.txt.0 should be present on the device ");
		LOGGER.info("**********************************************************************************");
		status = BroadBandCommonUtils.doesFileExistWithinGivenTimeFrameInArm(tapEnv, device,
			BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (status) {
		    LOGGER.info("STEP " + stepNum
			    + ": ACTUAL : The file /rdklogs/logs/telemetry2_0.txt.0 is present on the device");
		} else {
		    LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		/**
		 * STEP 3 : VERIFY IF 'DCMSCRIPT.LOG' INDICATES T2 IS ENABLED
		 */
		++stepNumber;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = "T2 enabled logs are not seen in /rdklogs/logs/dcmscript.log";
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP " + stepNumber + " :DESCRIPTION : Verify if 'dcmscript.log' indicates T2 is enabled ");
		LOGGER.info("STEP " + stepNumber
			+ " :ACTION : Execute the command , 'cat /rdklogs/logs/dcmscript.log' on the device ");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : The expected logs should be present on the file ");
		LOGGER.info("**********************************************************************************");
		logFileExists = CommonUtils.isFileExists(device, tapEnv,
			BroadBandCommandConstants.FILE_TMP_DCMSCRIPT_LOG);
		response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_CAT_RDKLOGS_DCMSCRIPT_LOG);
		status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
			BroadBandTestConstants.PATTERN_MATCHER_T2_IMPLEMENTATION);
		if (status) {
		    LOGGER.info("STEP " + stepNum
			    + ": ACTUAL : The file dcmscript.log indicates that T2 implementation is enabled on the device");
		} else {
		    if (logFileExists) {
			response = tapEnv.executeCommandUsingSsh(device,
				BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_CAT,
					BroadBandCommandConstants.FILE_TMP_DCMSCRIPT_LOG));
			status = CommonMethods.patternMatcher(response,
				BroadBandTestConstants.PATTERN_MATCHER_T2_IMPLEMENTATION);
		    }
		    if (status) {
			LOGGER.info("STEP " + stepNum
				+ ": ACTUAL : The file dcmscript.log indicates that T2 implementation is enabled on the device");
		    } else {
			LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		    }
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    } else {
		while (stepNumber <= BroadBandTestConstants.CONSTANT_3) {
		    stepNum = "S" + stepNumber;
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : Test step is applicable only for telemetry 2.0");
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		    ++stepNumber;
		}
	    }
	    /**
	     * STEP 4 : Verify setting the 2.4 GHz radio password to value: "password"
	     */
	    stepNumber = BroadBandTestConstants.CONSTANT_4;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Unable to set the 2.4 GHz Radio SSID Password";
	    LOGGER.info("*******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " :DESCRIPTION : Verify setting the 2.4 GHz radio password to value: 'password'");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Execute the webpa set command for"
		    + BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_2GHZ_PASSPHRASE
		    + " with value 'password'");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : WEBPA set should be successful for changing the 2.4GHz Password. ");
	    LOGGER.info("*******************************************************************************");
	    long startTime;
	    boolean iswifiFileExist;
	    do {
		startTime = System.currentTimeMillis();
		iswifiFileExist = CommonUtils.isFileExists(device, tapEnv,
			BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0);
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
		    && !iswifiFileExist);
	    if (iswifiFileExist) {
		tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.CMD_TAIL, BroadBandTestConstants.EMPTY_STRING,BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0 ,BroadBandTestConstants.STRING_GREATER_SYMBOL,BroadBandCommandConstants.FILE_TMP_WIFILOG,BroadBandTestConstants.CONSTANT_STRING_AMPERSAND));
	    }
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_2GHZ_PASSPHRASE,
		    BroadBandTestConstants.CONSTANT_0, "password");
	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : 2.4GHz RADIO SSID PASSWORD SET SUCCESSFULLY.");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 5 : Verify setting the 5 GHz radio password to value: "password"
	     */
	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Unable to set the 5 GHz Radio SSID Password";
	    LOGGER.info("*******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " :DESCRIPTION : Verify setting the 5GHz radio password to value: 'password'");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Execute the webpa set command for"
		    + BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_5GHZ_PASSPHRASE
		    + " with value 'password'");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : WEBPA set should be successful for changing the 5GHz Password. ");
	    LOGGER.info("*******************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_5GHZ_PASSPHRASE,
		    BroadBandTestConstants.CONSTANT_0, "password");
	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : 5GHz RADIO SSID PASSWORD SET SUCCESSFULLY.");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 6 : Verify the log message for same password set for both the radios.
	     */
	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Log Message for Same Password set for both radios is NOT present.";
	    LOGGER.info("*******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " :DESCRIPTION : Verify log message for same password set for both 2.4GHz and 5GHz Radios.");
	    LOGGER.info("STEP " + stepNumber
		    + " :ACTION : Execute command /rdklogs/logs/WiFilog.txt.0 and check for log message \"Same password was configured on User Private SSID for 2.4 and 5 GHz radios.\"");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : Expected Log message should be present in /rdklogs/logs/WiFilog.txt.0 file.");
	    LOGGER.info("*******************************************************************************");
	    logFileExists = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_TMP_WIFILOG);
	    startTime = System.currentTimeMillis();
	    do {
		tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		response = isAtomSyncAvailable
			? BroadBandCommonUtils.searchAtomConsoleLogs(tapEnv, device,
				BroadBandTraceConstants.LOG_MESSAGE_SAME_SSID_PASSWORD,
				BroadBandTestConstants.LOCATION_WIFI_LOG)
			: BroadBandCommonUtils.searchLogFiles(tapEnv, device,
				BroadBandTraceConstants.LOG_MESSAGE_SAME_SSID_PASSWORD,
				BroadBandTestConstants.LOCATION_WIFI_LOG);
		status = CommonMethods.isNotNull(response);
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS && !status);
	    if (!status && logFileExists) {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.CMD_CAT, BroadBandCommandConstants.FILE_TMP_WIFILOG));
		status = CommonUtils.isGivenStringAvailableInCommandOutput(response.trim(),
			BroadBandTraceConstants.LOG_MESSAGE_SAME_SSID_PASSWORD);
	    }
	    if (status) {
		LOGGER.info(
			"STEP " + stepNum + ": ACTUAL : LOG MESSAGE FOR SAME PASSWORD SET FOR BOTH RADIOS IS PRESENT.");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 7 : Verify CHANGING the 5 GHz radio password to value: "password_new"
	     */
	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Unable to set the 5 GHz Radio SSID Password";
	    LOGGER.info("*******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " :DESCRIPTION : Verify Changing the 5GHz Wifi Password via webpa with value as 'password_new'.");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Execute the webpa set command for"
		    + BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_5GHZ_PASSPHRASE
		    + " with value 'password_new'");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : WEBPA set should be successful for changing the 5GHz Password. ");
	    LOGGER.info("*******************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_5GHZ_PASSPHRASE,
		    BroadBandTestConstants.CONSTANT_0, "password_new");
	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : 5GHz RADIO SSID PASSWORD CHANGED SUCCESSFULLY.");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 8 : Verify the log message for different password set for the radios.
	     */
	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Log Message for Different Password set for the radios is NOT present.";
	    LOGGER.info("*******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " :DESCRIPTION : Verify log message for different password set for both 2.4GHz and 5GHz Radios.");
	    LOGGER.info("STEP " + stepNumber
		    + " :ACTION : Execute command /rdklogs/logs/WiFilog.txt.0 and check for log message \"Different password was configured on User Private SSID for 2.4 and 5 GHz radios.\"");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : Expected Log message should be present in /rdklogs/logs/WiFilog.txt.0 file.");
	    LOGGER.info("*******************************************************************************");
	    startTime = System.currentTimeMillis();
	    do {
		tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		response = isAtomSyncAvailable
			? BroadBandCommonUtils.searchAtomConsoleLogs(tapEnv, device,
				BroadBandTraceConstants.LOG_MESSAGE_DIFFERENT_SSID_PASSWORD,
				BroadBandTestConstants.LOCATION_WIFI_LOG)
			: BroadBandCommonUtils.searchLogFiles(tapEnv, device,
				BroadBandTraceConstants.LOG_MESSAGE_DIFFERENT_SSID_PASSWORD,
				BroadBandTestConstants.LOCATION_WIFI_LOG);
		status = CommonMethods.isNotNull(response);
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS && !status);
	    if (!status && logFileExists) {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.CMD_CAT, BroadBandCommandConstants.FILE_TMP_WIFILOG));
		status = CommonUtils.isGivenStringAvailableInCommandOutput(response.trim(),
			BroadBandTraceConstants.LOG_MESSAGE_DIFFERENT_SSID_PASSWORD);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNum
			+ ": ACTUAL : LOG MESSAGE FOR DIFFERENT PASSWORD SET FOR BOTH RADIOS IS PRESENT.");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    if (isTelemetryEnabled) {
		/**
		 * STEP 9 : VERIFY IF T2 MARKERS ARE GETTING UPLOADED TO SPLUNK
		 */
		++stepNumber;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = "Telemetry 2 markers are not getting uploaded to splunk";
		long startTimeStamp = System.currentTimeMillis();
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP " + stepNumber + " :DESCRIPTION : Verify if T2 markers are getting uploaded to splunk ");
		LOGGER.info("STEP " + stepNumber
			+ " :ACTION : Execute the command , 'cat /rdklogs/logs/telemetry2_0.txt.0' on the device ");
		LOGGER.info("STEP " + stepNumber
			+ " : EXPECTED : The logs like 'Report Sent Successfully over HTTP : 200' should present in /rdklogs/logs/telemetry2_0.txt.0 ");
		LOGGER.info("**********************************************************************************");

		do {
		    response = tapEnv.executeCommandUsingSsh(device,
			    BroadBandTestConstants.CMD_GET_REPORT_SENT_SUCCESS_MESSAGE_FROM_LOGS);
		    status = CommonMethods.isNotNull(response)
			    && response.contains(BroadBandTestConstants.STRING_REPORT_SENT);
		} while (!status
			&& (System.currentTimeMillis()
				- startTimeStamp) < BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

		if (status) {
		    LOGGER.info("STEP " + stepNum
			    + ": ACTUAL : Telemetry 2 markers are getting uploaded to splunk successfully");
		} else {
		    LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		/**
		 * STEP 10 : VERIFY IF LOGS ARE PRESENT IN SPLUNK
		 */
		++stepNumber;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = "Telemetry 2 logs are not present in splunk";
		startTimeStamp = System.currentTimeMillis();
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Verify if T2 logs are present in splunk ");
		LOGGER.info("STEP " + stepNumber
			+ " :ACTION : Perform search on splunk portal with the device's estb mac and the log upload time ");
		LOGGER.info("STEP " + stepNumber
			+ " : EXPECTED : The logs should be present with the appropriate timestamp ");
		LOGGER.info("**********************************************************************************");
		if (!isTelemetryEnabled) {
		if (BroadbandPropertyFileHandler.isSplunkEnabled()) {
			do {
		    response = BroadBandTelemetry2Utils.retrieveTelemetryLogsFromSplunk(device, tapEnv, null);
		    status = CommonMethods.isNotNull(response);
		} while (!status
			&& (System.currentTimeMillis()
				- startTimeStamp) < BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
		}
		  if (!BroadbandPropertyFileHandler.isSplunkEnabled()) {

				LOGGER.info("Splunk is disabled");
				LOGGER.info("Skipping the step :");
				errorMessage = "failed to get the log as Splunk is disabled";
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
					 errorMessage, false);

				LOGGER.info("STEP 10" + " : ACTUAL : " + errorMessage);
			    } else{
		if (status) {
		    LOGGER.info("STEP " + stepNum
			    + ": ACTUAL : Telemetry 2 markers are getting uploaded to splunk successfully");
		} else {
		    LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			    }
		}
		/**
		 * STEP 11 : Verify the telemetry marker for same password set for both for radios.
		 */
		stepNumber = BroadBandTestConstants.CONSTANT_11;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = "Telemetry Marker for Same Password for both radios is NOT present in splunk logs.";
		LOGGER.info("*******************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ " :DESCRIPTION : Verify telemetry marker for same password set for both radios are present in splunk ");
		LOGGER.info("STEP " + stepNumber
			+ " :ACTION : Perform search on splunk log with value as 'SAME_PASSWORDS_PRIVATE_RADIOS'");
		LOGGER.info("STEP " + stepNumber
			+ " : EXPECTED : Splunk Logs should contain the marker 'SAME_PASSWORDS_PRIVATE_RADIOS' as expected");
		LOGGER.info("*******************************************************************************");
		List<String> verifySplunkLog = new ArrayList<>();
		JSONObject telemetryPayloadData = BroadBandTelemetryUtils.getPayLoadDataAsJson(tapEnv, device,
			BroadBandTraceConstants.TELEMETRY_MARKER_SAME_SSID_PASSWORD, true);
		LOGGER.info("SEARCHED THE DCMSCRIPT.LOG FOR TELEMETRY MARKER FOR SAME SSID PASSWORD: "
			+ (null != telemetryPayloadData));
		if (null != telemetryPayloadData) {
		    response = BroadBandTelemetryUtils.getPayloadParameterValue(telemetryPayloadData,
			    BroadBandTraceConstants.TELEMETRY_MARKER_SAME_SSID_PASSWORD);
		    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils.compareValues("INT_NON_ZERO",
			    BroadBandTestConstants.STRING_VALUE_ONE, response);
		    errorMessage = "Telemetry Marker for Same Password set for both radios is present BUT with inappropriate value";
		}
		response = BroadBandTelemetry2Utils.retrieveTelemetryLogsFromSplunk(device, tapEnv, null);
		status = response.contains(BroadBandTraceConstants.TELEMETRY_MARKER_SAME_SSID_PASSWORD);
		if (!status) {
		    verifySplunkLog.add(BroadBandTraceConstants.TELEMETRY_MARKER_SAME_SSID_PASSWORD);
		    startTime = System.currentTimeMillis();
		    do {
			LOGGER.info("Waiting for device to verify telemetry status");
			status = BroadBandTelemetryUtils.verifyTelemetryDataPayLoadFromSplunk(device, tapEnv,
				verifySplunkLog);
		    } while (!status
			    && (System.currentTimeMillis()
				    - startTime) < BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS
			    && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS));
		}
		LOGGER.info("*******************************************************************************");
		if (status) {
		    LOGGER.info("STEP " + stepNum
			    + ": ACTUAL : Splunk Logs contains the marker 'SAME_PASSWORDS_PRIVATE_RADIOS' as expected");
		} else {
		    LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("*******************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		/**
		 * STEP 12 : Verify the telemetry marker for different password set for both for radios.
		 */
		++stepNumber;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = "Telemetry Marker for Different Password for both radios is NOT present in splunk logs.";
		LOGGER.info("*******************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ " :DESCRIPTION : Verify telemetry marker for different password set for both radios are present in splunk ");
		LOGGER.info("STEP " + stepNumber
			+ " :ACTION : Perform search on splunk log with value as 'DIFFERENT_PASSWORDS_PRIVATE_RADIOS'");
		LOGGER.info("STEP " + stepNumber
			+ " : EXPECTED : Splunk Logs should contain the marker 'DIFFERENT_PASSWORDS_PRIVATE_RADIOS' as expected");
		LOGGER.info("*******************************************************************************");
		LOGGER.info("GOING TO SEARCH THE DCMSCRIPT.LOG FOR TELEMETRY MARKER FOR DIFFERENT SSID PASSWORD.");
		telemetryPayloadData = BroadBandTelemetryUtils.getPayLoadDataAsJson(tapEnv, device,
			BroadBandTraceConstants.TELEMETRY_MARKER_DIFFERENT_SSID_PASSWORD, false);
		LOGGER.info("SEARCHED THE DCMSCRIPT.LOG FOR TELEMETRY MARKER FOR DIFFERENT SSID PASSWORD: "
			+ (null != telemetryPayloadData));
		if (null != telemetryPayloadData) {
		    response = BroadBandTelemetryUtils.getPayloadParameterValue(telemetryPayloadData,
			    BroadBandTraceConstants.TELEMETRY_MARKER_DIFFERENT_SSID_PASSWORD);
		    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils.compareValues("INT_NON_ZERO",
			    BroadBandTestConstants.STRING_VALUE_ONE, response);
		    errorMessage = "Telemetry Marker for Different Password set for the radios is present BUT with inappropriate value.";
		}

		if (!status) {
		    verifySplunkLog.add(BroadBandTraceConstants.TELEMETRY_MARKER_DIFFERENT_SSID_PASSWORD);
		    startTime = System.currentTimeMillis();
		    do {
			LOGGER.info("Waiting for device to verify telemetry status");
			status = BroadBandTelemetryUtils.verifyTelemetryDataPayLoadFromSplunk(device, tapEnv,
				verifySplunkLog);
		    } while (!status
			    && (System.currentTimeMillis()
				    - startTime) < BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS
			    && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS));
		}
		if (status) {
		    LOGGER.info("STEP " + stepNum
			    + ": ACTUAL : Splunk Logs contains the marker 'DIFFERENT_PASSWORDS_PRIVATE_RADIOS' as expected");
		} else {
		    LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("*******************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    } else {
		++stepNumber;
		while (stepNumber <= BroadBandTestConstants.CONSTANT_12) {
		    stepNum = "S" + stepNumber;
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : Test step not applicable for telemetry 2.0");
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		    ++stepNumber;
		}
	    }
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING THE MARKERS IN SPLUNK LOGS: " + errorMessage);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	}
    }
}

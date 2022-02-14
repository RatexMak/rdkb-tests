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
package com.automatics.rdkb.tests.security;

import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandSecurityTest extends AutomaticsTestBase {

    /**
     * @TestDetails
     * 
     *              Test for verify the OpenSSL version used in the build. OpenSSL version used must be the latest
     *              available OpenSSL version.
     * 
     *              The build is expected to use latest available version of OpenSSL
     * 
     *              <ol>
     *              <li>STEP 1: Execute the command openSSL version and note the response
     *              <li>EXPECTED: Command should return latest openSSL version
     * 
     *              <li>STEP 2: Execute command to check the version of OpenSSL present in libssl.so
     *              <li>EXPECTED: libssl.so should contain the latest Available OpenSSL version
     * 
     *              <li>STEP 3: Execute the command to get the openSSL version used in libcrypto.so
     *              <li>EXPECTED: libcrypto.so should contain the latest Available OpenSSL version
     * 
     *              <li>STEP 4: Execute the command to get the openSSL version used in libssl.so used by all processes
     *              <li>EXPECTED: All processes should use latest available OpenSSL version
     * 
     *              <li>STEP 5: Execute command to check the version of OpenSSL present in libcrypto.so library used by
     *              all processes
     *              <li>EXPECTED: All processes should use latest available OpenSSL version
     *              </ol>
     * 
     *              NOTE:Latest OpenSSL version must be updated in properties file.Latest version can be obtained from
     *              openSSL.org
     * 
     * @param device
     *            The device to be used.
     * @author rahul raveendran
     * @refactored said hisham
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    TestGroup.SECURITY }, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-SECURITY-1001")
    public void testToVerifyOpenSslVersion(Dut device) {

	// Holds the test case ID.
	String testId = "TC-RDKB-SECURITY-001";
	// Error message
	String message = null;
	// Test step number
	String step = "s1";
	// Variable to hold the server response
	String response = null;
	// boolean variable to store the status of test step
	boolean status = false;
	// string to get the latest openSSL version from properties file
	String latestOpensslVersion = null;
	/**
	 * PRE-CONDITION :VERIFY OPEN SSL VERSION IS AVAILABLE IN PROPERTY FILE
	 */
	LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	LOGGER.info("#######################################################################################");
	LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Retieve SSL version from stb properties ");
	LOGGER.info("PRE-CONDITION 1 : ACTION : GET the values from stb roperties for key: openSSL.version ");
	LOGGER.info("PRE-CONDITION 1 : EXPTECTED : Successfully retrieved the Open SSL version");
	LOGGER.info("#######################################################################################");
	message = "Unable to retrieve the OPENSSL version from stb properties";
	try {
	    latestOpensslVersion = BroadbandPropertyFileHandler.getLatestOpenSSLVersion();
	    status = CommonMethods.isNotNull(latestOpensslVersion);
	} catch (Exception e) {
	    LOGGER.info("Exception occured while fetching open ssl version from stb properties" + e);
	}
	if (status) {
	    LOGGER.info(
		    "PRE-CONDITION 1 : ACTUAL : Successfully retrieved the Open SSL version from the property file.");
	} else {
	    LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + message);
	    throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : FAILED : " + message);
	}
	LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	try {
	    /*
	     * STEP 1: Execute the command openSSL version and note the response " EXPECTED: Command should return
	     * latestopenSSL version
	     */
	    LOGGER.info("STARTING TEST CASE--TC-RDKB-SECURITY-1001");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: Execute the command openSSL version and note the response ");
	    LOGGER.info("EXPECTED: Command should return latest openSSL version ");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.COMMAND_FOR_OPENSSL_VERSION);
	    LOGGER.info("LATEST OpenSSL version is : " + latestOpensslVersion);

	    if (CommonMethods.isNotNull(response) && response.contains(latestOpensslVersion)) {
		status = true;
		LOGGER.info("Build contains latest available openSSL version " + response);
	    } else {
		message = "Build does not contain latest available openSSL version";
		LOGGER.error(message);

	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, message, true);

	    /*
	     * STEP 2: Execute the command to get the openSSL version used in libssl.so " EXPECTED: Command should
	     * return latest openSSL version
	     */
	    step = "s2";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: Execute command to check the version of OpenSSL present in libssl.so");
	    LOGGER.info("EXPECTED: Command should return latest openSSL version");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.COMMAND_TO_FETCH_LIBSSL_FILE);
	    LOGGER.info("LIBSSL used in build is: " + response);
	    if (CommonMethods.isNotNull(response)) {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandTestConstants.PREFIX_COMMAND_TO_READ_OPENSSL_OF_SSL_LIBRARY
				+ BroadBandTestConstants.SINGLE_SPACE_CHARACTER + response.trim()
				+ BroadBandTestConstants.SINGLE_SPACE_CHARACTER
				+ BroadBandTestConstants.POSTFIX_COMMAND_TO_READ_OPENSSL_OF_SSL_LIBRARY);

		LOGGER.info("openSSL version used in libssl is: " + response);

		if (CommonMethods.isNotNull(response) && response.contains(latestOpensslVersion)) {
		    status = true;
		    LOGGER.info("libssl.so contains latest available openSSL version");
		} else {
		    message = "libssl.so donot contain latest available openSSL version";
		    LOGGER.error(message);

		}
	    } else {
		message = "Failed in executing command to  fetch libssl";
		LOGGER.error(message);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	    /*
	     * STEP 3: Execute the command to get the openSSL version used in libcrypto.so " EXPECTED: Command should
	     * return latest openSSL version
	     */
	    step = "s3";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: Execute command to check the version of OpenSSL present in libcrypto.so");
	    LOGGER.info("EXPECTED: Command should return latest openSSL version");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.COMMAND_TO_FETCH_LIBCRYPTO_FILE);
	    LOGGER.info("libcrypto used in build is : " + response);
	    if (CommonMethods.isNotNull(response)) {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandTestConstants.PREFIX_COMMAND_TO_READ_OPENSSL_OF_SSL_LIBRARY
				+ BroadBandTestConstants.SINGLE_SPACE_CHARACTER + response.trim()
				+ BroadBandTestConstants.SINGLE_SPACE_CHARACTER
				+ BroadBandTestConstants.POSTFIX_COMMAND_TO_READ_OPENSSL_OF_SSL_LIBRARY);
		LOGGER.info("openSSL version used in libcrypto is: " + response);
		if (CommonMethods.isNotNull(response) && response.contains(latestOpensslVersion)) {
		    status = true;
		    LOGGER.info("libcrypto.so contains latest available openSSL version");
		} else {
		    message = "libcrypto.so donot contain latest available openSSL version";
		    LOGGER.error(message);

		}
	    } else {
		message = "Failed in executing command to fetch libcrypto";
		LOGGER.error(message);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	    /*
	     * STEP 4: Execute the command to get the openSSL version used in libssl.so used by all processes" EXPECTED:
	     * Command should return latest openSSL version
	     */
	    step = "s4";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: Execute command to check the version of OpenSSL present in libssl.so library used by all processes");
	    LOGGER.info("EXPECTED: Command should return latest openSSL version");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.COMMAND_TO_GET_LIBSSL_USED_IN_ALL_PROCESSES);
	    if (CommonMethods.isNotNull(response)) {
		response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TO_GET_LIBSSL);
		if (CommonMethods.isNotNull(response)) {
		    LOGGER.info("libssl used by all processes : " + response);
		    response = tapEnv.executeCommandUsingSsh(device,
			    BroadBandTestConstants.PREFIX_COMMAND_TO_READ_OPENSSL_OF_SSL_LIBRARY
				    + BroadBandTestConstants.SINGLE_SPACE_CHARACTER + response.trim()
				    + BroadBandTestConstants.SINGLE_SPACE_CHARACTER
				    + BroadBandTestConstants.POSTFIX_COMMAND_TO_READ_OPENSSL_OF_SSL_LIBRARY);
		    LOGGER.info("openSSL version used in libssl of all processes is: " + response);
		    if (CommonMethods.isNotNull(response) && response.contains(latestOpensslVersion)) {
			status = true;
			LOGGER.info("libssl.so used by all processes contains latest available openSSL version");
		    } else {
			message = "libssl.so used by processes donot contain latest available openSSL version";
			LOGGER.error(message);

		    }
		} else {
		    message = "Null response received from Pattern finder for LIBSSL";
		    LOGGER.error(message);
		}
	    } else {
		message = "Failed in fetching libssl used by all processes";
		LOGGER.error(message);

	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	    /*
	     * STEP 5: Execute the command to get the openSSL version used in libcrypto.so used by all processes"
	     * EXPECTED: Command should return latest openSSL version
	     */
	    step = "s5";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: Execute command to check the version of OpenSSL present in libcrypto.so library used by all processes");
	    LOGGER.info("EXPECTED: Command should return latest openSSL version");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.COMMAND_TO_GET_LIBCRYPTO_USED_IN_ALL_PROCESSES);
	    if (CommonMethods.isNotNull(response)) {
		response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TO_GET_LIBCRYPTO);
		if (CommonMethods.isNotNull(response)) {
		    LOGGER.info("libcrypto used by all processes : " + response);
		    response = tapEnv.executeCommandUsingSsh(device,
			    BroadBandTestConstants.PREFIX_COMMAND_TO_READ_OPENSSL_OF_SSL_LIBRARY
				    + BroadBandTestConstants.SINGLE_SPACE_CHARACTER + response.trim()
				    + BroadBandTestConstants.SINGLE_SPACE_CHARACTER
				    + BroadBandTestConstants.POSTFIX_COMMAND_TO_READ_OPENSSL_OF_SSL_LIBRARY);
		    LOGGER.info("openSSL version used in libcrypto of all processes is: " + response);

		    if (CommonMethods.isNotNull(response) && response.contains(latestOpensslVersion)) {
			status = true;
			LOGGER.info("libcrypto.so used by all processes contains latest available openSSL version");
		    } else {
			message = "libssl.so used by processes donot contain latest available openSSL version";
			LOGGER.error(message);
		    }
		} else {
		    message = "NULL response obtained from pattern finder f0r Libcrypto";
		    LOGGER.error(message);
		}
	    } else {
		message = "Failed in getting libcrypto used by all processes";
		LOGGER.error(message);

	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, message, true);

	} catch (Exception exception) {

	    LOGGER.error("Exception caught while executing tests for RDKB " + exception.getMessage());
	    tapEnv.updateExecutionStatus(device, testId, step, status, exception.getMessage(), true);
	}
	LOGGER.info("ENDING TEST CASE--TC-RDKB-SECURITY-1001");
    }

    /**
     * Verify third party tracker is blocked after firewall restart
     * <ol>
     * <li>Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable as True.</li>
     * <li>Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable to True.</li>
     * <li>Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable to FALSE</li>
     * <li>Verify the log string "AdTrackerBlockingRFCEnable:FALSE" in /rdklogs/logs/PAMlog.txt.0</li>
     * <li>Verify the WebPA Sync notifications log for PrivacyProtection.Enable to FALSE</li>
     * <li>Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to True</li>
     * <li>Reboot the device and wait for IP acquisition</li>
     * <li>Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable persists as FALSE
     * after reboot</li>
     * <li>Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_ PrivacyProtection.Activate persists as TRUE after
     * reboot</li>
     * <li>Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable to TRUE</li>
     * <li>Verify the log string "AdTrackerBlockingRFCEnable:TRUE" in /rdklogs/logs/PAMlog.txt.0</li>
     * <li>Verify the WebPA Sync notifications log for PrivacyProtection.Enable to TRUE</li>
     * <li>Restart the firewall</li>
     * </ol>
     * 
     * @author dnalui917
     * @refactor said hisham
     * 
     * 
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SECURITY-2203")
    public void testToVerifyThirdPartyTrackerIsBlockedAfterRestart(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SECURITY-223";
	String stepNum = "s1";
	String errorMessage = null;
	String response = null;
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SECURITY-2203");
	LOGGER.info("TEST DESCRIPTION: Verify third party tracker is blocked after firewall restart");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable as True. ");
	LOGGER.info(
		"2. Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable to True. ");
	LOGGER.info("3. Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable to FALSE");
	LOGGER.info("4. Verify the log string \"AdTrackerBlockingRFCEnable:FALSE\" in /rdklogs/logs/ADVSEClog.txt.0");
	LOGGER.info("5. Verify the WebPA Sync notifications log for PrivacyProtection.Enable to FALSE");
	LOGGER.info("6. Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to True");
	LOGGER.info("7. Reboot the device and wait for IP acquisition");
	LOGGER.info(
		"8. Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable persists as FALSE after reboot");
	LOGGER.info(
		"9. Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_ PrivacyProtection.Activate persists as TRUE after reboot");
	LOGGER.info("10. Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable to TRUE");
	LOGGER.info("11. Verify the log string \"AdTrackerBlockingRFCEnable:TRUE\" in /rdklogs/logs/ADVSEClog.txt.0");
	LOGGER.info("12. Verify the WebPA Sync notifications log for PrivacyProtection.Enable to TRUE");
	LOGGER.info("13. Restart the firewall");

	LOGGER.info("#######################################################################################");

	try {

	    executeCommonPreConditionSteps(device, tapEnv, testCaseId);

	    stepNum = "s3";
	    errorMessage = "Parameter value is not set & retrieved as false";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable to FALSE");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>:<MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\" Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable\",\"value\":\"false\"}]}\"");
	    LOGGER.info("STEP 3: EXPECTED : Parameter value should be set & retrieved as false");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Successfully verified Parameter value is set & retrieved as false");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify the log string \"AdTrackerBlockingRFCEnable:FALSE\" in /rdklogs/logs/ADVSEClog.txt.0");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the following command:grep -i \"AdTrackerBlockingRFCEnable:FALSE\"  /rdklogs/logs/ADVSEClog.txt.0");
	    LOGGER.info("STEP 4: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.ADTRACKER_BLOCKING_RFC_ENABLE, BroadBandTestConstants.FILE_ADVSEC_0,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.FALSE.toUpperCase());

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully verified the log message");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify the WebPA Sync notifications log for PrivacyProtection.Enable to FALSE");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the following command:grep -i \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable\" /rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info("STEP 5: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ENABLE,
		    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully verified the log message");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Parameter value is not set & retrieved as True";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to True");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>:<MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\" Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate\",\"value\":\"true\"}]}\"");
	    LOGGER.info("STEP 6: EXPECTED : Parameter value should be set & retrieved as True");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ACTIVATE, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully verified that Parameter value is set & retrieved as True");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Failed to Reboot the device and wait for IP acquisition";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Reboot the device and wait for IP acquisition");
	    LOGGER.info("STEP 7: ACTION : Execute the command:   /sbin/reboot");
	    LOGGER.info("STEP 7: EXPECTED : Device should be rebooted");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully rebooted the device");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Failed to verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable persists as FALSE after reboot";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable persists as FALSE after reboot");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute the following command:curl -H \"\"Authorization: Bearer <SAT_TOKEN>\"\" -k <WEBPA URL>:<ECM_MAC>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable");
	    LOGGER.info(
		    "STEP 8: EXPECTED : Value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable should persist as FALSE after reboot");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ENABLE, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Successfully verified parameter persists as False after reboot");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Failed to verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_ PrivacyProtection.Activate persists as TRUE after reboot";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_ PrivacyProtection.Activate persists as TRUE after reboot");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the following command:curl -H \"\"Authorization: Bearer <SAT_TOKEN>\"\" -k <WEBPA URL>:<ECM_MAC>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_ PrivacyProtection.Activate");
	    LOGGER.info(
		    "STEP 9: EXPECTED : value of Device.DeviceInfo.X_RDKCENTRAL-COM_ PrivacyProtection.Activate should persist  as TRUE after reboot");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ACTIVATE, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Successfully verified parameter persists as TRUE after reboot");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "Parameter value is not set & retrieved as True";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable to TRUE");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>:<MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\" Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable\",\"value\":\"true\"}]}\"");
	    LOGGER.info("STEP 10: EXPECTED : Parameter value should be set & retrieved as True");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Successfully verified that Parameter value is set & retrieved as True");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Verify the log string \"AdTrackerBlockingRFCEnable:TRUE\" in /rdklogs/logs/ADVSEClog.txt.0");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the following command:grep -i \"AdTrackerBlockingRFCEnable:TRUE\" /rdklogs/logs/ADVSEClog.txt.0");
	    LOGGER.info("STEP 11: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.ADTRACKER_BLOCKING_RFC_ENABLE, BroadBandTestConstants.FILE_ADVSEC_0,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    LOGGER.info("Response from advsec file" + response);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.TRUE.toUpperCase());

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Successfully retrieved the log message");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify the WebPA Sync notifications log for PrivacyProtection.Enable to TRUE");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute the following command:grep -i \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PrivacyProtection.Enable\" /rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info("STEP 12: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ENABLE,
		    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Successfully retrieved the log message");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s13";
	    errorMessage = "Failed to  Restart the firewall";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Restart the firewall");
	    LOGGER.info("STEP 13: ACTION : Execute the following command:sysevent set firewall-restart");
	    LOGGER.info("STEP 13: EXPECTED : Restart of firewall should be successful");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods
		    .isNull(tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_REFRESH_FIREWALL));

	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Successfully Restarted the firewall");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    // RABID PROCESS & CUJO IS NOT APPLICABLE FOR RDKB
	    // SKIPPING TESTNO 14,15 AND 16

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    executeCommonPostConditionSteps(device, tapEnv);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-SECURITY-2203");
    }

    public void executeCommonPreConditionSteps(Dut device, AutomaticsTapApi tapEnv, String testCaseId) {

	// Variable Declaration begins
	String stepNum = "s1";
	String errorMessage = null;
	boolean status = false;
	String safeBrowsing = null;
	// Variable Declaration Ends

	LOGGER.info("**********************************************************************************");

	stepNum = "s1";
	errorMessage = "Parameter value is not retrieved as True";
	status = false;

	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 1: DESCRIPTION : Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable as True. ");
	LOGGER.info(
		"STEP 1: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>:<MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\" Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\",\"value\":\"true\"}]}\"");
	LOGGER.info("STEP 1: EXPECTED : Parameter value should be retrieved as True");
	LOGGER.info("**********************************************************************************");

	status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE, BroadBandTestConstants.TRUE,
		BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	if (!status) {
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE);
	}

	if (status) {
	    LOGGER.info("STEP 1: ACTUAL : Successfully verified Parameter value is retrieved as true");
	} else {
	    LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	}

	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	LOGGER.info("**********************************************************************************");

	stepNum = "s2";
	errorMessage = "Parameter value is not set & retrieved as True";
	status = false;

	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 2: DESCRIPTION : Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable to True. ");
	LOGGER.info(
		"STEP 2: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>:<MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\" Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable\",\"value\":\"true\"}]}\"");
	LOGGER.info("STEP 2: EXPECTED : Parameter value should be set & retrieved as True");
	LOGGER.info("**********************************************************************************");

	long startTime = System.currentTimeMillis();
	if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
	    try {
		do {
		    safeBrowsing = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SAFE_BROWSING);
		    if (safeBrowsing.equalsIgnoreCase(BroadBandTestConstants.FALSE)
			    || safeBrowsing.equalsIgnoreCase(BroadBandTestConstants.TRUE)) {
			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SAFE_BROWSING,
				WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
		    }
		} while (!status
			&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    } catch (Exception e) {
		LOGGER.error("Exception occured while setting safe browsing " + e.getMessage());
	    }

	} else {
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SAFE_BROWSING,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	}

	if (status) {
	    LOGGER.info("STEP 2: ACTUAL : Successfully verified Parameter value is set & retrieved as true");
	} else {
	    LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	}

	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	LOGGER.info("**********************************************************************************");
    }

    public void executeCommonPostConditionSteps(Dut device, AutomaticsTapApi tapEnv) {

	// Variable Declaration begins
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	LOGGER.info("POST-CONDITION STEPS");
	LOGGER.info(
		"POST-CONDITION : DESCRIPTION : Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable to FALSE.");
	LOGGER.info(
		"POST-CONDITION : ACTION : Execute the following command:curl -H \\\"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>:<MAC>/config -d \\\"{\\\"parameters\\\":[{\\\"dataType\\\":0,\\\"name\\\":\\\" Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable\\\",\\\"value\\\":\\\"false\\\"}]}\\\"\");\r\n"
			+ "		");
	LOGGER.info("POST-CONDITION : EXPECTED : Parameter value should be set & retrieved as FALSE");

	status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SAFE_BROWSING, WebPaDataTypes.BOOLEAN.getValue(),
		BroadBandTestConstants.FALSE);

	if (status) {
	    LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	} else {
	    LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	}
	LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

    }

    /**
     * @TestDetails Verifing the dropbear version of the device
     * 
     *              The dropbear version of the device is expected to be the latest version as mentioned in properties
     *              file (stb.properties)
     * 
     *              <p>
     *              STEPS:
     *              <ol>
     *              <li>STEP 1: Execute command to check the dropbear version of device using SSH connection to ARM
     *              <li>EXPECTED: Command should return latest dropbear version
     *              <li>STEP 2: Execute command to check the dropbear version of device using SSH connection to ATOM
     *              <li>EXPECTED: Command should return latest dropbear version
     * 
     *              NOTE:Latest dropbear version must be updated in properties file(latest.dropbear.version)
     * 
     *              </ol>
     * @param device
     *            The device to be used.
     * @author Susheela C
     * @refactor yamini.s
     * 
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    TestGroup.SECURITY }, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-SECURITY-2001")
    public void testDropbearVersion(Dut device) {
	// Holds the test case ID.
	String testCaseId = "TC-RDKB-SECURITY-201";
	// boolean variable to store the status of test step1
	boolean status = false;
	// Variable to hold the server response
	String response = null;
	// Test step number
	String stepNumber = "s1";
	// Error message
	String errorMessage = null;
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SECURITY-2001");
	    LOGGER.info("TEST DESCRIPTION: Verifying the Dropbear Version of the device");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("Step 1. Execute command to check the dropbear version of device using SSH connection to ARM");
	    LOGGER.info("Step 2. Execute command to check the dropbear version of device using SSH connection to ATOM");
	    LOGGER.info("#######################################################################################");
	    /**
	     * STEP 1 : EXECUTE COMMAND TO CHECK THE DROPBEAR VERSION OF DEVICE USING SSH CONNECTION TO ARM
	     */
	    errorMessage = "The Device doesn't have the latest Dropbear Version in ARM Console";
	    // string to get the latest dropbear version from properties file

	    String latestDropbearVersion = BroadbandPropertyFileHandler.getLatestDropbearVersion();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Execute command to check the dropbear version of Device using SSH to ARM connection");
	    LOGGER.info("STEP 1: ACTION : EXECUTE THE COMMAND, \"/usr/sbin/dropbear -V\"");
	    LOGGER.info("STEP 1: EXPECTED: Command should return the latest dropbear version");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_DROPBEAR_VERSION);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.patternSearchFromTargetString(response, latestDropbearVersion);
	    if (status) {
		LOGGER.info("STEP 1 : ACTUAL : Successfully returned the latest dropbear version");
	    } else {
		LOGGER.error("STEP 1 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    /**
	     * STEP 2 : EXECUTE COMMAND TO CHECK THE DROPBEAR VERSION OF DEVICE USING SSH CONNECTION TO ATOM
	     */

	    // Test step number
	    stepNumber = "s2";
	    // boolean variable to store the status of test step2
	    status = false;
	    errorMessage = "The Device doesn't have the latest Dropbear Version in ATOM Console";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Execute command to check the dropbear version of Device using SSH to ATOM connection");
	    LOGGER.info("STEP 2: ACTION : EXECUTE THE COMMAND, \"/usr/sbin/dropbear -V\"");
	    LOGGER.info("STEP 2: EXPECTED: Command should return the latest dropbear version");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		response = CommonMethods.executeCommandInAtomConsole(device, tapEnv,
			BroadBandCommandConstants.CMD_GET_DROPBEAR_VERSION);
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.patternSearchFromTargetString(response, latestDropbearVersion);
		if (status) {
		    LOGGER.info("STEP 2 : ACTUAL : Successfully returned the latest dropbear version");
		} else {
		    LOGGER.error("STEP 2 : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    } else {
		errorMessage = "Test step is not applicable for devices model " + device.getModel();
		LOGGER.error("STEP 2 : ACTUAL : " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured while validating dropbear version of the device " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SECURITY-2001");
    }

    /**
     * Verify DBus -Overly permissive system setting.
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify "/etc/dbus-1/system-local.conf" file is not present. Note - Regression tests needs to be verified
     * manually, that there is no negative effect with this change.</li>
     * </ol>
     * 
     * @author Praveen Kumar P, ppaner200
     * @refactor yamini.s
     * @param device
     * @ {@link Dut}
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1101")
    public void testDbusAuthenticationChange(Dut device) {

	String testCaseId = "TC-RDKB-SYSTEM-101";
	String stepNumber = "s1";
	boolean status = false;
	String errorMessage = null;
	try {
	    /**
	     * Step 1 : Verify "/etc/dbus-1/system-local.conf" file is not present
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Step 1 : Verify \"/etc/dbus-1/system-local.conf\" file is not present."
		    + "Note - Regression tests needs to be verified manually, that there is no negative effect with this change.");
	    LOGGER.info("Expected Result - \"system-local.conf\" file should not be present in /etc/dbus-1/.");
	    LOGGER.info("**********************************************************************************");

	    status = !CommonUtils.isFileExists(device, tapEnv, BroadBandTestConstants.FILE_SYSTEM_LOCAL_CONF);
	    LOGGER.info("is file \"system-local.conf\" present in folder /etc/dbus-1/ - " + !status);
	    errorMessage = " File \"system-local.conf\" is present in folder /etc/dbus-1/";
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING DEFAULT DBUS - OVERLY PERMISSIVE SYSTEM SETTING : "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info(
		    "POST EXECUTION - Regression tests needs to be verified manually, that there is no negative effect due to \"system-local.conf\" file removal.");
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-1101");

    }

    /**
     * 
     * Test Case : To Verify lighttpd and php fingerprinting
     * 
     * <li>1.Verify server.tag in lighttpd.conf</li>
     * <li>2.Verify expose_php value in php.ini</li>
     * 
     * @author Deepa Bada
     * @Refactor Sruthi Santhosh
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    TestGroup.NEW_FEATURE, TestGroup.SECURITY })
    @TestDetails(testUID = "TC-RDKB-SECURITY-1105")
    public void testToVerifyLighttpdAndPhpFingerprinting(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SECURITY-1105");
	LOGGER.info("TEST DESCRIPTION: To Verify lighttpd and php fingerprinting ");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify server.tag in lighttpd.conf ");
	LOGGER.info("2. Verify expose_php value in php.ini ");

	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-SECURITY-115";
	boolean isAtom = false;
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// String to store dibbler version
	String response = null;
	// variable declaration ends
	isAtom = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	try {

	    stepNumber = "s1";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION: Verify server.tag in lighttpd.conf ");
	    LOGGER.info("STEP 1: ACTION: Execute Command : grep -i \"server.tag\" /etc/lighttpd.conf ");
	    LOGGER.info(
		    "STEP 1: EXPECTED: server.tag = \"Xfinity Broadband Router Server\" is available in lighttpd conf ");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Unable to verify server tag in lighttpd conf file";
	    response = isAtom ? BroadBandCommonUtils.searchArmOrAtomLogFile(tapEnv, device,
		    BroadBandTestConstants.CONSTANT_SERVER_DOT_TAG, BroadBandTestConstants.CONSTANT_LIGHTTPD_CONF)
		    : BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTestConstants.CONSTANT_SERVER_DOT_TAG,
			    BroadBandTestConstants.CONSTANT_LIGHTTPD_CONF);
	    if (CommonMethods.isNotNull(response)) {
		response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_SERVER_TAG);
		status = CommonMethods.isNotNull(response)
			&& response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_SERVER_TAG);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL :Successfully Verified server tag in lighttpd conf file ");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    // ##################################################################################################//

	    stepNumber = "s2";
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION: Verify expose_php value in php.ini");
	    LOGGER.info("STEP 2: ACTION: Execute Command : grep -i \"expose_php\" /etc/php.ini");
	    LOGGER.info("STEP 2: EXPECTED: expose_php value is off in php.ini file");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Unable to verify expose php value in php.ini file";
	    if (DeviceModeHandler.isBusinessClassDevice(device)) {
		response = isAtom
			? BroadBandCommonUtils.searchArmOrAtomLogFile(tapEnv, device,
				BroadBandTestConstants.CONSTANT_EXPOSE_PHP, BroadBandTestConstants.CONSTANT_PHP_INI)
			: BroadBandCommonUtils.searchLogFiles(tapEnv, device,
				BroadBandTestConstants.CONSTANT_EXPOSE_PHP, BroadBandTestConstants.CONSTANT_PHP_INI);
		status = CommonMethods.isNotNull(response)
			&& response.trim().equalsIgnoreCase(BroadBandTestConstants.CONSTANT_EXPOSE_PHP_VALUE);
		if (status) {
		    LOGGER.info("STEP 2: ACTUAL :Successfully verified expose php value in php.ini file  ");
		} else {
		    LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    } else {
		errorMessage = "Except Bussiness Class devices , all models have migrated from PHP to JST. Hence this step is Marked as NA";
		LOGGER.info(errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	    // ##################################################################################################//

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying lighttpd and php fingerprinting " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SECURITY-1105");
    }

    /**
     * Verify whether the upgraded PHP version is 7.2.13 or above
     * <ol>
     * <li>Verify whether the upgraded PHP version is 7.2.13 or above</li>
     * </ol>
     * 
     * @author Geetha DS
     * @Refactor Sruthi Santhosh
     **/
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SECURITY-1106")
    public void testToVerifyPHPVersion(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SECURITY-116";
	String stepNum = "s1";
	String errorMessage = null;
	boolean status = false;
	String response = "";
	String response1 = "";
	int status1 = 0;

	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SECURITY-1106");
	LOGGER.info("TEST DESCRIPTION: Verify whether the upgraded PHP version is 7.2.13 or above");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify whether the upgraded PHP version is 7.2.13 or above");

	LOGGER.info("#######################################################################################");

	try {

	    errorMessage = "PHP version obtained is not as expected";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify whether the upgraded PHP version is 7.2.13 or above");
	    LOGGER.info("STEP 1: ACTION : Execute command: php-cgi --version ");
	    LOGGER.info("STEP 1: EXPECTED : PHP version must be 7.2.13 or above");
	    LOGGER.info("**********************************************************************************");

	    response1 = AutomaticsTapApi.getSTBPropsValue(BroadBandTestConstants.PHP_VERSION);

	    // to execute php-cgi --version command to read the latest version
	    // of PHP
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		response = CommonMethods.executeCommandInAtomConsole(device, tapEnv,
			BroadBandCommandConstants.CMD_TO_GET_LATEST_PHP_VERSION);
	    } else {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CMD_TO_GET_LATEST_PHP_VERSION);
	    }
	    LOGGER.info("Expected version: " + response1 + " or above");
	    LOGGER.info("Actual Version: " + response);

	    if (CommonMethods.isNotNull(response) && CommonMethods.isNotNull(response1)) {
		status1 = CommonUtils.versionComparison(response.trim(), response1.trim());
	    } else {
		LOGGER.error("Unable to fetch PHP version");
	    }

	    status = (status1 >= 0);

	    if (status) {
		LOGGER.info("STEP:1 PHP version obtained is the latest one");
	    } else {
		LOGGER.error("STEP:1 " + errorMessage + " : " + response);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**************************************************************************");
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, false, errorMessage,
		    false);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-SECURITY-1106");
    }

    /**
     * Testcase for securing syscfg.db parameters via encryption
     * <ol>
     * <li>Verify if "SEC: syscfg.db stored in/opt/secure/data" log message is logged in
     * /rdklogs/logs/Consolelog.txt.0</li>
     * <li>Verify if by default, Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram is set as True</li>
     * <li>Verify if syscfg.db file is present in both /nvram and /opt/secure/data folders</li>
     * <li>Verify if syscfg.db file in both /nvram and /opt/secure/data folders is identical</li>
     * <li>Verify if a parameter is set to the default value in the syscfg.db file.</li>
     * <li>Verify modifying a parameter in the syscfg.db file</li>
     * <li>Verify if the changes have reflected in the /nvram/syscfg.db file</li>
     * <li>Verify if the changes have reflected in the /opt/secure/data/syscfg.db file</li>
     * </ol>
     * 
     * @author Taher Veeramgoanwala
     * @refactor yamini.s
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    TestGroup.SECURITY }, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-SECURITY-1002")
    public void testToVerifySecureSyscfgdb(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SECURITY-002";
	String stepNum = "s1";
	String errorMessage = null;
	boolean status = false;
	String response = null;
	boolean securedSyscfg = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SECURITY-1002");
	LOGGER.info("TEST DESCRIPTION: Testcase for securing syscfg.db parameters via encryption");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1. Verify if \"SEC: syscfg.db stored in /opt/secure/data\" or \"SEC: syscfg.db stored in /nvram/data\" log message is logged in /rdklogs/logs/Consolelog.txt.0");
	LOGGER.info(
		"2. Verify if by default, Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram is set as True");
	LOGGER.info("3. Verify if syscfg.db file is present in both /nvram and /opt/secure/data folders");
	LOGGER.info("4. Verify if syscfg.db file in both /nvram and /opt/secure/data folders is identical");
	LOGGER.info("5. Verify if a parameter is set to the default value in the syscfg.db file.");
	LOGGER.info("6. Verify modifying a parameter in the syscfg.db file");
	LOGGER.info("7. Verify if the changes have reflected in the /nvram/syscfg.db file ");
	LOGGER.info("8. Verify if the changes have reflected in the /opt/secure/data/syscfg.db file ");
	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify if \"SEC: syscfg.db stored in /opt/secure/data\" or \"SEC: syscfg.db stored in /nvram/data\" log message is logged in /rdklogs/logs/Consolelog.txt.0");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the command:grep \"syscfg.db stored in\" /rdklogs/logs/Consolelog.txt.0");
	    LOGGER.info(
		    "STEP 1: EXPECTED : Should return \"SEC: syscfg.db stored in /opt/secure/data\" or \"SEC: syscfg.db stored in /nvram/data\" as response.");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Device didn't comes up after reboot";
	    if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
		errorMessage = "Expected message is not present in Consolelog.txt.0 or ArmConsolelog.txt.0";

		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_SYSCFG_MESSAGE_STORED_IN_NVRAM,
			(CommonMethods.isAtomSyncAvailable(device, tapEnv)
				? BroadBandCommandConstants.FILE_ARMCONSOLELOG
				: BroadBandCommandConstants.FILE_CONSOLELOG),
			BroadBandTestConstants.SIX_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
		if (!status) {
		    String logMsg = BroadBandCommonUtils.concatStringUsingStringBuffer(
			    BroadBandTraceConstants.LOG_MESSAGE_SYSCFG_MESSAGE_STORED_IN_SECURE_DATA,
			    BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG);
		    securedSyscfg = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device, logMsg,
			    (CommonMethods.isAtomSyncAvailable(device, tapEnv)
				    ? BroadBandCommandConstants.FILE_ARMCONSOLELOG
				    : BroadBandCommandConstants.FILE_CONSOLELOG),
			    BroadBandTestConstants.SIX_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));

		    status = securedSyscfg;
		}
	    }

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : \"SEC: syscfg.db stored in /opt/secure/data\" or \"SEC: syscfg.db stored in /nvram/data\" is logged successfully");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s2";
	    errorMessage = "Failed to get the response as \"True\".";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify if by default, Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram is set as True");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA URL>/api/v2/device/mac:<MAC>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram");
	    LOGGER.info("STEP 2: EXPECTED : The value should be \"True\".");
	    LOGGER.info("**********************************************************************************");
	    if (securedSyscfg) {
		LOGGER.info("STEP 2: NA for this model as the param is removed");
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);

	    } else {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_SYSCFG_UPDATE_NVRAM, BroadBandTestConstants.CONSTANT_3,
			BroadBandTestConstants.TRUE);

		if (status) {
		    LOGGER.info("STEP 2: ACTUAL : SysCfg.UpdateNvram is enabled by default");
		} else {
		    LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    stepNum = "s3";
	    status = false;
	    if (!securedSyscfg) {
		errorMessage = "Failed to verify the syscfg.db file in the /nvram folder.";
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 3: DESCRIPTION : Verify if syscfg.db file is present in both /nvram and /opt/secure/data folders");
		LOGGER.info(
			"STEP 3: ACTION : Execute the commands: a) ls -ltr /nvram/syscfg.db b) ls -ltr /opt/secure/data/syscfg.db");
		LOGGER.info("STEP 3: EXPECTED : Syscfg.db file should be preset in both the folders.");
		LOGGER.info("**********************************************************************************");

		if (CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOG_FILE_SYSCFG)) {
		    errorMessage = "Failed to verify the syscfg.db file in the /opt/secure/data folder.";
		    status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG);
		}

		if (status) {
		    LOGGER.info("STEP 3: ACTUAL : Syscfg.db files are present in both the folders ");
		} else {
		    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
		}

	    } else {
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 3: DESCRIPTION : Verify if syscfg.db file is present in /opt/secure/data folders");
		LOGGER.info("STEP 3: ACTION : Execute the command: ls -ltr /opt/secure/data/syscfg.db");
		LOGGER.info("STEP 3: EXPECTED : Syscfg.db file should be preset in the folder.");
		LOGGER.info("**********************************************************************************");

		errorMessage = "Failed to verify the syscfg.db file in the /opt/secure/data folder.";
		status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG);

		if (status) {
		    LOGGER.info("STEP 3: ACTUAL : Syscfg.db file is present in the /opt/secure/data folder ");
		} else {
		    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
		}
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s4";
	    errorMessage = "Failed to verify the syscfg.db file is identical in both the folders.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify if syscfg.db file in both /nvram and /opt/secure/data folders are identical");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the commands:a) ls -ltr /nvram/syscfg.db b) ls -ltr /opt/secure/data/syscfg.db");
	    LOGGER.info("STEP 4: EXPECTED : Syscfg.db file should be identical in both the folders.");
	    LOGGER.info("**********************************************************************************");
	    if (securedSyscfg) {
		LOGGER.info("STEP 4: NA for this device model as syscfg.db file is removed from nvram folder");
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);

	    } else {
		status = CommonMethods.isNull(
			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_CONFIG_SYSCFG_DIFFERENCE));

		if (status) {
		    LOGGER.info("STEP 4: ACTUAL : Syscfg.db files are identical in both the folders.");
		} else {
		    LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    stepNum = "s5";
	    errorMessage = "Failed to get the response as \"Low\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify if a parameter is set to the default value in the syscfg.db file.");
	    LOGGER.info("STEP 5: ACTION : Execute the command: syscfg get firewall_level");
	    LOGGER.info("STEP 5: EXPECTED :firewall_level  parameter value should be \"Low\"");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_CONFIG_FIREWALL_LEVEL);
	    status = CommonMethods.isNotNull(response)
		    && response.trim().equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_LOW);

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : firewall_level is Low");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s6";
	    errorMessage = "Failed to set the value to \"High\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify modifying a parameter in the syscfg.db file");
	    LOGGER.info("STEP 6: ACTION : Execute the commands:a) syscfg set firewall_level High b) syscfg commit");
	    LOGGER.info("STEP 6: EXPECTED : The value should be set to \"High\"");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_CONFIG_SET_FIREWALL_LEVEL_HIGH));

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : firewall_level is High");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s7";
	    errorMessage = "Failed to return the value as \"High\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify if the changes have reflected in the /nvram/syscfg.db file ");
	    LOGGER.info("STEP 7: ACTION : Execute the command:grep firewall_level /nvram/syscfg.db");
	    LOGGER.info("STEP 7: EXPECTED : The value of firewall_level parameter should be returned as \"High\"");
	    LOGGER.info("**********************************************************************************");
	    if (securedSyscfg) {
		LOGGER.info("STEP 7: NA for this device model as syscfg file is removed from nvram folder");
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);

	    } else {
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_SYSCFG_FIREWALL, BroadBandCommandConstants.LOG_FILE_SYSCFG,
			BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

		if (status) {
		    LOGGER.info(
			    "STEP 7: ACTUAL : The value of firewall_level parameter is reflected as High in /nvram/syscfg.db");
		} else {
		    LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    stepNum = "s8";
	    errorMessage = "Failed to return the value as \"High\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify if the changes have reflected in the /opt/secure/data/syscfg.db file ");
	    LOGGER.info("STEP 8: ACTION : Execute the command: grep firewall_level /opt/secure/data/syscfg.db");
	    LOGGER.info("STEP 8: EXPECTED : The value of firewall_level parameter should be returned as \"High\"");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_SYSCFG_FIREWALL,
		    BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : The value of firewall_level parameter is reflected as High in /opt/secure/data/syscfg.db");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, false, errorMessage,
		    false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION : DESCRIPTION : Change the value of \"firewall_level\" to low");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : Execute the commands:a) syscfg set firewall_level Lowb) syscfg commit");
	    LOGGER.info("POST-CONDITION : EXPECTED : Post condition success");

	    status = CommonMethods.isNull(
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_CONFIG_SET_FIREWALL_LEVEL_LOW));

	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SECURITY-1002");
    }

    /**
     * Verify third party tracker is not blocked
     * <ol>
     * <li>Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable as True.</li>
     * <li>Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable to True.</li>
     * <li>Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable to FALSE</li>
     * <li>Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to True</li>
     * <li>Verify the Verify log message PRIVACY_PROTECTION_ACTIVATED in agent.txt</li>
     * <li>Verify the WebPA Sync notifications log for PrivacyProtection.Activate to TRUE</li>
     * <li>Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to False</li>
     * <li>Verify the Verify log message PRIVACY_PROTECTION_DEACTIVATED in agent.txt</li>
     * <li>Verify the WebPA Sync notifications log for PrivacyProtection.Activate to FALSE</li>
     * </ol>
     * 
     * @author Dipankur Nalui
     * @refactor yamini.s
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    TestGroup.SECURITY }, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-SECURITY-2201")
    public void testToVerifyThirdPartyTrackerIsNotBlocked(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SECURITY-221";
	String stepNum = "s1";
	String errorMessage = null;
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SECURITY-2201");
	LOGGER.info("TEST DESCRIPTION: Verify third party tracker is not blocked");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable as True.");
	LOGGER.info(
		"2. Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable to True. ");
	LOGGER.info("3. Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable to FALSE");
	LOGGER.info("4. Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to True");
	LOGGER.info("5. Verify the Verify log message PRIVACY_PROTECTION_ACTIVATED in agent.txt");
	LOGGER.info("6. Verify the WebPA Sync notifications log for PrivacyProtection.Activate to TRUE");
	LOGGER.info("7. Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to False");
	LOGGER.info("8. Verify the Verify log message PRIVACY_PROTECTION_DEACTIVATED in agent.txt");
	LOGGER.info("9. Verify the WebPA Sync notifications log for PrivacyProtection.Activate to FALSE");

	LOGGER.info("#######################################################################################");

	try {

	    executeCommonPreConditionSteps(device, tapEnv, testCaseId);

	    stepNum = "s3";
	    errorMessage = "Parameter value is not set & retrieved as false";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable to FALSE");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the following command: curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>/api/v2/device/mac:7894B4F3F778/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable\",\"value\":\"false\"}]}\"");
	    LOGGER.info("STEP 3: EXPECTED : Parameter value should be set & retrieved as false");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_CODEBIG_FIRST_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Successfully verifed that Parameter value is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Parameter value is not set & retrieved as True";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to True");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>/api/v2/device/mac:<MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\" Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate\",\"value\":\"true\"}]}\"");
	    LOGGER.info("STEP 4: EXPECTED : Parameter value should be set & retrieved as True");
	    LOGGER.info("**********************************************************************************");

	    if ((CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_AGENT))
		    && (CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_WEBPA))) {
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ACTIVATE,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
			BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
	    }

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully verifed that Parameter value is retrieved as True");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify the Verify log message PRIVACY_PROTECTION_ACTIVATED in agent.txt");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the following command:grep -i PRIVACY_PROTECTION_ACTIVATED /rdklogs/logs/agent.txt");
	    LOGGER.info("STEP 5: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.STRING_PRIVACY_PROTECTION_ACTIVATED,
		    BroadBandCommandConstants.LOG_FILE_AGENT, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully verifed the log message");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify the WebPA Sync notifications log for PrivacyProtection.Activate to TRUE");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the following command:grep -i \"Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate\" /rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info("STEP 6: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ACTIVATE,
		    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully verifed the log message");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Parameter value is not set & retrieved as False";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to False");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>/api/v2/device/mac:<MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\" Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate\",\"value\":\"false\"}]}\"");
	    LOGGER.info("STEP 7: EXPECTED : Parameter value should be set & retrieved as False");
	    LOGGER.info("**********************************************************************************");

	    if ((CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_AGENT))
		    && (CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_WEBPA))) {
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ACTIVATE,
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully verifed that Parameter value is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify the Verify log message PRIVACY_PROTECTION_DEACTIVATED in agent.txt");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute the following command:grep -i PRIVACY_PROTECTION_DEACTIVATED /rdklogs/logs/agent.txt");
	    LOGGER.info("STEP 8: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.STRING_PRIVACY_PROTECTION_DEACTIVATED,
		    BroadBandCommandConstants.LOG_FILE_AGENT, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Successfully verifed the log message");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify the WebPA Sync notifications log for PrivacyProtection.Activate to FALSE");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the following command:grep -i \"Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate\" /rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info("STEP 9: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ACTIVATE,
		    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Successfully verifed the log message");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    executeCommonPostConditionSteps(device, tapEnv);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-SECURITY-2201");
    }

    /**
     * Verify third party tracker is blocked
     * <ol>
     * <li>Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable as True.</li>
     * <li>Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable to True.</li>
     * <li>Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable to FALSE</li>
     * <li>Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to True.</li>
     * <li>Verify log message PRIVACY_PROTECTION_ACTIVATED in agent.txt</li>
     * <li>Verify the WebPA Sync notifications log for PrivacyProtection.Activate to TRUE</li>
     * <li>Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to False.</li>
     * <li>Verify log message PRIVACY_PROTECTION_DEACTIVATED in agent.txt</li>
     * <li>Verify the WebPA Sync notifications log for PrivacyProtection.Activate to FALSE</li>
     * </ol>
     * 
     * @author Dipankur Nalui
     * @refactor yamini.s
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    TestGroup.SECURITY }, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-SECURITY-2202")
    public void testToVerifyThirdPartyTrackerIsBlocked(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SECURITY-222";
	String stepNum = "s1";
	String errorMessage = null;
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SECURITY-2202");
	LOGGER.info("TEST DESCRIPTION: Verify third party tracker is blocked");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the value of Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable as True.");
	LOGGER.info(
		"2. Set and verify Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable to True. ");
	LOGGER.info("3. Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable to FALSE");
	LOGGER.info("4. Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to True. ");
	LOGGER.info("5. Verify log message  PRIVACY_PROTECTION_ACTIVATED in agent.txt");
	LOGGER.info("6. Verify the WebPA Sync notifications log for PrivacyProtection.Activate to TRUE");
	LOGGER.info("7. Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to False.");
	LOGGER.info("8. Verify log message  PRIVACY_PROTECTION_DEACTIVATED in agent.txt");
	LOGGER.info("9. Verify the WebPA Sync notifications log for PrivacyProtection.Activate to FALSE");

	LOGGER.info("#######################################################################################");

	try {

	    executeCommonPreConditionSteps(device, tapEnv, testCaseId);

	    stepNum = "s3";
	    errorMessage = "Parameter value is not set & retrieved as false";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable to FALSE");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the following command: curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>/api/v2/device/mac:7894B4F3F778/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable\",\"value\":\"false\"}]}\"");
	    LOGGER.info("STEP 3: EXPECTED : Parameter value should be set & retrieved as false");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_CODEBIG_FIRST_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Successfully verifed that Parameter value is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Parameter value is not set & retrieved as True";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to True. ");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>/api/v2/device/mac:<MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\" Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate\",\"value\":\"true\"}]}\"");
	    LOGGER.info("STEP 4: EXPECTED : Parameter value should be set & retrieved as True");
	    LOGGER.info("**********************************************************************************");

	    if ((CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_AGENT))
		    && (CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_WEBPA))) {
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ACTIVATE,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
			BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
	    }

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully verifed that Parameter value is retrieved as True");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify log message  PRIVACY_PROTECTION_ACTIVATED in agent.txt");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the following command:grep -i PRIVACY_PROTECTION_ACTIVATED /rdklogs/logs/agent.txt");
	    LOGGER.info("STEP 5: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.STRING_PRIVACY_PROTECTION_ACTIVATED,
		    BroadBandCommandConstants.LOG_FILE_AGENT, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully verifed the log message");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify the WebPA Sync notifications log for PrivacyProtection.Activate to TRUE");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the following command:grep -i \"Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate\" /rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info("STEP 6: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ACTIVATE,
		    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully verifed the log message");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Parameter value is not set & retrieved as false";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Set Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate to False.");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>/api/v2/device/mac:<MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\" Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate\",\"value\":\"false\"}]}\"");
	    LOGGER.info("STEP 7: EXPECTED : Parameter value should be set & retrieved as false");
	    LOGGER.info("**********************************************************************************");

	    if ((CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_AGENT))
		    && (CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_WEBPA))) {
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ACTIVATE,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
			BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
	    }

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully verifed that Parameter value is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify log message  PRIVACY_PROTECTION_DEACTIVATED in agent.txt");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute the following command: grep -i PRIVACY_PROTECTION_DEACTIVATED /rdklogs/logs/agent.txt");
	    LOGGER.info("STEP 8: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.STRING_PRIVACY_PROTECTION_DEACTIVATED,
		    BroadBandCommandConstants.LOG_FILE_AGENT, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Successfully verifed the log message");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Failed to retrieve the log message";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify the WebPA Sync notifications log for PrivacyProtection.Activate to FALSE");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the following command: grep -i \"Device.DeviceInfo.X_RDKCENTRAL-COM_PrivacyProtection.Activate\" /rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info("STEP 9: EXPECTED : log message should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_PRIVACY_PROTECTION_ACTIVATE,
		    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Successfully verifed the log message");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    executeCommonPostConditionSteps(device, tapEnv);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SECURITY-2202");
    }

    /**
     * Verify syscfg parameters by enabling and disabling SysCfg.UpdateNvram
     * <ol>
     * <li>Enable Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram via Webpa</li>
     * <li>Verify whether "Syscfg stored in /nvram/syscfg.db" log message is available in Consolelog.txt.0 or
     * ArmConsolelog.txt.0</li>
     * <li>Verify whether syscfg.db is available in /tmp, /nvram and /opt/secure/data</li>
     * <li>Verify syscfg show output comes from the content of /opt/secure/data/syscfg.db</li>
     * <li>Update and verify syscfg show output comes from the content of /opt/secure/data/syscfg.db for persistence
     * check</li>
     * <li>Disable Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram via Webpa</li>
     * <li>Verify whether "Syscfg stored in /opt/secure/data/syscfg.db" log message is available in Consolelog.txt.0 or
     * ArmConsolelog.txt.0 when UpdateNvram is false</li>
     * <li>Verify whether /nvram/syscfg.db is not available when UpdateNvram is false</li>
     * <li>Verify syscfg show output comes from the content of /opt/secure/data/syscfg.db when UpdateNvram is false</li>
     * <li>Update and verify syscfg show output comes from the content of /opt/secure/data/syscfg.db for persistence
     * check when UpdateNvram is false</li>
     * <li>Verify the persistence of values after reboot</li>
     * <li>Verify whether default values are obtained after removing /opt/secure/data/syscfg.db</li>
     * 
     * @author Dipankar Nalui
     * @refactor Said Hisham
     *           </ol>
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSCFG-1000")
    public void testToVerifySyscfgParametersByEnablingAndDisablingUpdateNvram(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SYSCFG-100";
	String stepNum = "s1";
	String errorMessage = null;
	boolean status = false;
	/** Command to check SNMPV3 support using syscfg */
	String cmdSyscfgShowSnmpv3Support = BroadBandCommonUtils.concatStringUsingStringBuffer(
		BroadBandCommandConstants.CMD_SYSCFG_SHOW, BroadBandTestConstants.SYMBOL_PIPE_WITH_SPACES,
		BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.STRING_SNMP_V3SUPPORT);

	/** Command to check firewall_level6 using syscfg */
	String cmdSyscfgShowFirewallLevelv6 = BroadBandCommonUtils.concatStringUsingStringBuffer(
		BroadBandCommandConstants.CMD_SYSCFG_SHOW, BroadBandTestConstants.SYMBOL_PIPE_WITH_SPACES,
		BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.STRING_FIREWALL_LEVEL_V6);

	/** Command to check UPdateNvram using syscfg */
	String cmdSyscfgShowUpdateNvram = BroadBandCommonUtils.concatStringUsingStringBuffer(
		BroadBandCommandConstants.CMD_SYSCFG_SHOW, BroadBandTestConstants.SYMBOL_PIPE_WITH_SPACES,
		BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.STRING_UPDATE_NVRAM);
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSCFG-1000");
	LOGGER.info("TEST DESCRIPTION: Verify syscfg parameters by enabling and disabling SysCfg.UpdateNvram");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Enable Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram via Webpa");
	LOGGER.info(
		"2. Verify whether \"Syscfg stored in /nvram/syscfg.db\" log message is available in Consolelog.txt.0 or ArmConsolelog.txt.0");
	LOGGER.info("3. Verify whether syscfg.db is available in /tmp, /nvram and /opt/secure/data");
	LOGGER.info("4. Verify syscfg show output comes from the content of /opt/secure/data/syscfg.db");
	LOGGER.info(
		"5. Update and verify syscfg show output comes from the content of /opt/secure/data/syscfg.db for persistence check");
	LOGGER.info("6. Disable Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram via Webpa");
	LOGGER.info(
		"7. Verify whether \"Syscfg stored in /opt/secure/data/syscfg.db\" log message is available in Consolelog.txt.0 or ArmConsolelog.txt.0 when UpdateNvram is false");
	LOGGER.info("8. Verify whether /nvram/syscfg.db is not available when UpdateNvram is false");
	LOGGER.info(
		"9. Verify syscfg show output comes from the content of /opt/secure/data/syscfg.db when UpdateNvram is false");
	LOGGER.info(
		"10. Update and verify syscfg show output comes from the content of /opt/secure/data/syscfg.db for persistence check when UpdateNvram is false");
	LOGGER.info("11. Verify the persistence of values after reboot");
	LOGGER.info("12. Verify whether default values are obtained after removing /opt/secure/data/syscfg.db");

	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "s1";
	    errorMessage = "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram is not enabled via Webpa";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Enable Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram via Webpa");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the following command: 1. curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>/api/v2/device/mac:<ECM_MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\"dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram\",\"value\":\"true\"}]}\"2. /sbin/reboot3. curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA URL>/api/v2/device/mac:<ECM_MAC>/config?names=dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram");
	    LOGGER.info(
		    "STEP 1: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram should be enabled via Webpa");
	    LOGGER.info("**********************************************************************************");

	    if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_SYSCFG_UPDATE_NVRAM, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE)) {
		errorMessage = "reboot was not successful";
		if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
		    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_SYSCFG_UPDATE_NVRAM, BroadBandTestConstants.TRUE,
			    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		}
	    }

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram is enabled via Webpa");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "\"Syscfg stored in /nvram/syscfg.db\" log message is not available in Consolelog.txt.0 or ArmConsolelog.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify whether \"Syscfg stored in /nvram/syscfg.db\" log message is available in Consolelog.txt.0 or ArmConsolelog.txt.0");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the following command: grep -i \"Syscfg stored in /nvram/syscfg.db\" /rdklogs/logs/Consolelog.txt.0	grep -i  \"Syscfg stored in /nvram/syscfg.db\" /rdklogs/logs/ArmConsolelog.txt.0");
	    LOGGER.info(
		    "STEP 2: EXPECTED : \"Syscfg stored in /nvram/syscfg.db\" log message should be available in Consolelog.txt.0 or ArmConsolelog.txt.0");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_SYSCFG_STORED_IN_NVRAM,
		    (CommonMethods.isAtomSyncAvailable(device, tapEnv) ? BroadBandCommandConstants.FILE_ARMCONSOLELOG
			    : BroadBandCommandConstants.FILE_CONSOLELOG),
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : \"Syscfg stored in /nvram/syscfg.db\" log message is available in Consolelog.txt.0 or ArmConsolelog.txt.0");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "syscfg.db is not available in /tmp, /nvram and /opt/secure/data";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify whether syscfg.db is available in /tmp, /nvram and /opt/secure/data");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the following command: ls -ltr /tmp/syscfg.db  /opt/secure/data/syscfg.db /nvram/syscfg.db");
	    LOGGER.info("STEP 3: EXPECTED : syscfg.db should be available in /tmp, /nvram and /opt/secure/data");
	    LOGGER.info("**********************************************************************************");

	    if (CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOG_FILE_SYSCFG)) {
		LOGGER.info(BroadBandCommandConstants.LOG_FILE_SYSCFG + " is available");
		errorMessage = "Failed to verify the syscfg.db file in the /opt/secure/data folder.";
		if (CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG)) {
		    LOGGER.info(BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG + " is available");
		    errorMessage = "Failed to verify the syscfg.db file in the /opt/secure/data folder.";
		    status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOG_FILE_TMP_SYSCFG);
		}
	    }

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : syscfg.db is available in /tmp, /nvram and /opt/secure/data");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "\"syscfg show\" output is not same as the content of /opt/secure/data/syscfg.db";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify \"syscfg show\" output comes from the content of /opt/secure/data/syscfg.db");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the following command: 1. syscfg show | grep -i V3Support 2. grep -i V3Support /opt/secure/data/syscfg.db 3. syscfg show | grep -i firewall_level 4. grep -i firewall_level /opt/secure/data/syscfg.db 5. syscfg show | grep -i UPdateNvram 6. grep -i UPdateNvram /opt/secure/data/syscfg.db");
	    LOGGER.info(
		    "STEP 4: EXPECTED : \"syscfg show\" should come from the content of /opt/secure/data/syscfg.db");
	    LOGGER.info("**********************************************************************************");

	    /* compare syscfg show output with the content of syscfg.db file */
	    status = compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv, cmdSyscfgShowSnmpv3Support,
		    BroadBandTestConstants.STRING_SNMP_V3SUPPORT)
		    && compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv, cmdSyscfgShowFirewallLevelv6,
			    BroadBandTestConstants.STRING_FIREWALL_LEVEL_V6)
		    && compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv, cmdSyscfgShowUpdateNvram,
			    BroadBandTestConstants.STRING_UPDATE_NVRAM);

	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : \"syscfg show\" output is same as the content of /opt/secure/data/syscfg.db");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "After updating, \"syscfg show\" output is not same as the content of /opt/secure/data/syscfg.db";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Update and verify \"syscfg show\" output comes from the content of /opt/secure/data/syscfg.db for persistence check");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the following commands: 	1. syscfg set V3Support false; syscfg commit 2. syscfg set firewall_levelv6 Custom; syscfg commit  3. syscfg show | grep -i V3Support 4. grep -i V3Support /opt/secure/data/syscfg.db  5. syscfg show | grep -i firewall_level 6. grep -i firewall_level /opt/secure/data/syscfg.db  7. syscfg show | grep -i UPdateNvram 8. grep -i UPdateNvram /opt/secure/data/syscfg.db");
	    LOGGER.info(
		    "STEP 5: EXPECTED : After updating, \"syscfg show\" output should come from the content of /opt/secure/data/syscfg.db");
	    LOGGER.info("**********************************************************************************");

	    /* update V3Support to false */
	    if (CommonMethods.isNull(
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_CONFIG_DISABLEV3SUPPORT))) {
		/* update firewall_levelv6 to Custom */
		errorMessage = "Failed to execute the command"
			+ BroadBandCommandConstants.CMD_TO_SET_FIREWALL_LEVEL_V6_TO_CUSTOM;
		if (CommonMethods.isNull(tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CMD_TO_SET_FIREWALL_LEVEL_V6_TO_CUSTOM))) {

		    status = compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv,
			    cmdSyscfgShowSnmpv3Support, BroadBandTestConstants.STRING_SNMP_V3SUPPORT)
			    && compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv,
				    cmdSyscfgShowFirewallLevelv6, BroadBandTestConstants.STRING_FIREWALL_LEVEL_V6)
			    && compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv,
				    cmdSyscfgShowUpdateNvram, BroadBandTestConstants.STRING_UPDATE_NVRAM);
		}

	    }

	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : After updating, \"syscfg show\" output is same as the content of /opt/secure/data/syscfg.db");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram is not disabled via Webpa";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Disable Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram via Webpa");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the following command: 1. curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>/api/v2/device/mac:<ECM_MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\"dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram\",\"value\":\"false\"}]}\"2. /sbin/reboot3. curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA URL>/api/v2/device/mac:<ECM_MAC>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram should be disabled via Webpa");
	    LOGGER.info("**********************************************************************************");

	    if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_SYSCFG_UPDATE_NVRAM, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.FALSE)) {
		errorMessage = "reboot was not successful";
		if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
		    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_SYSCFG_UPDATE_NVRAM, BroadBandTestConstants.FALSE,
			    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		}
	    }

	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram is disabled via Webpa");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "\"Syscfg stored in /opt/secure/data/syscfg.db\" log message is not available in Consolelog.txt.0 or ArmConsolelog.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify whether \"Syscfg stored in /opt/secure/data/syscfg.db\" log message is available in Consolelog.txt.0 or ArmConsolelog.txt.0 when UpdateNvram is false");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute the following command: 1. grep -i  \"Syscfg stored in /opt/secure/data/syscfg.db\" /rdklogs/logs/Consolelog.txt.02. 	grep -i  \"Syscfg stored in /opt/secure/data/syscfg.db\" /rdklogs/logs/ArmConsolelog.txt.0");
	    LOGGER.info(
		    "STEP 7: EXPECTED : \"Syscfg stored in /opt/secure/data/syscfg.db\" log message should be available in Consolelog.txt.0 or ArmConsolelog.txt.0");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_SYSCFG_STORED_IN_OPT_SECURE_DATA,
		    (CommonMethods.isAtomSyncAvailable(device, tapEnv) ? BroadBandCommandConstants.FILE_ARMCONSOLELOG
			    : BroadBandCommandConstants.FILE_CONSOLELOG),
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL : \"Syscfg stored in /opt/secure/data/syscfg.db\" log message is available in Consolelog.txt.0 or ArmConsolelog.txt.0");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = " /nvram/syscfg.db is available when UpdateNvram is false";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify whether /nvram/syscfg.db is not available when UpdateNvram is false");
	    LOGGER.info("STEP 8: ACTION : Execute the following command: 	ls -ltr /nvram/syscfg.db");
	    LOGGER.info("STEP 8: EXPECTED :  /nvram/syscfg.db should not be available when UpdateNvram is false");
	    LOGGER.info("**********************************************************************************");

	    status = !CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOG_FILE_SYSCFG);

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : /nvram/syscfg.db is not available when UpdateNvram is false");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "\"syscfg show\" output is not same as the content of /opt/secure/data/syscfg.db";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify \"syscfg show\" output comes from the content of /opt/secure/data/syscfg.db when UpdateNvram is false");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the following command: 1. syscfg show | grep -i V3Support 2. grep -i V3Support /opt/secure/data/syscfg.db 3. syscfg show | grep -i firewall_level 4. grep -i firewall_level /opt/secure/data/syscfg.db 5. syscfg show | grep -i UPdateNvram 6. grep -i UPdateNvram /opt/secure/data/syscfg.db");
	    LOGGER.info(
		    "STEP 9: EXPECTED : \"syscfg show\" output should come from the content of /opt/secure/data/syscfg.db");
	    LOGGER.info("**********************************************************************************");

	    /* compare syscfg show output with the content of syscfg.db file */
	    status = compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv, cmdSyscfgShowSnmpv3Support,
		    BroadBandTestConstants.STRING_SNMP_V3SUPPORT)
		    && compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv, cmdSyscfgShowFirewallLevelv6,
			    BroadBandTestConstants.STRING_FIREWALL_LEVEL_V6)
		    && compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv, cmdSyscfgShowUpdateNvram,
			    BroadBandTestConstants.STRING_UPDATE_NVRAM);

	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : \"syscfg show\" output is same as the content of /opt/secure/data/syscfg.db");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "After updating, \"syscfg show\" output is not same as the content of /opt/secure/data/syscfg.db";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Update and verify \"syscfg show\" output comes from the content of /opt/secure/data/syscfg.db for persistence check when UpdateNvram is false");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute the following commands: 	1. syscfg set V3Support true; syscfg commit 2. syscfg set firewall_levelv6 High; syscfg commit  3. syscfg show | grep -i V3Support 4. grep -i V3Support /opt/secure/data/syscfg.db  5. syscfg show | grep -i firewall_level 6. grep -i firewall_level /opt/secure/data/syscfg.db  7. syscfg show | grep -i UPdateNvram 8. grep -i UPdateNvram /opt/secure/data/syscfg.db");
	    LOGGER.info(
		    "STEP 10: EXPECTED : After updating, \"syscfg show\" output should come from the content of /opt/secure/data/syscfg.db");
	    LOGGER.info("**********************************************************************************");

	    /* update V3Support to true */
	    if (CommonMethods.isNull(
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_CONFIG_ENABLEV3SUPPORT))) {
		/* update firewall_levelv6 to High */
		errorMessage = "Failed to execute the command"
			+ BroadBandCommandConstants.CMD_TO_SET_FIREWALL_LEVEL_V6_TO_HIGH;
		if (CommonMethods.isNull(tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CMD_TO_SET_FIREWALL_LEVEL_V6_TO_HIGH))) {

		    /* compare syscfg show output with the content of syscfg.db file */
		    status = compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv,
			    cmdSyscfgShowSnmpv3Support, BroadBandTestConstants.STRING_SNMP_V3SUPPORT)
			    && compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv,
				    cmdSyscfgShowFirewallLevelv6, BroadBandTestConstants.STRING_FIREWALL_LEVEL_V6)
			    && compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(device, tapEnv,
				    cmdSyscfgShowUpdateNvram, BroadBandTestConstants.STRING_UPDATE_NVRAM);
		}
	    }

	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : After updating, \"syscfg show\" output is same as the content of /opt/secure/data/syscfg.db");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Failed to verify updated values does not persist after reboot";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify the persistence of values after reboot");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the following command: 	1. reboot 2. curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA URL>/api/v2/device/mac:<ECM_MAC>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram	3. ls -ltr /tmp/syscfg.db  /opt/secure/data/syscfg.db /nvram/syscfg.db	4. grep \"Syscfg stored in\" /rdklogs/logs/Consolelog.txt.0	5. syscfg show | grep -i firewall_level	6. syscfg show | grep -i V3Support	7. syscfg show | grep -i UPdateNvram");
	    LOGGER.info("STEP 11: EXPECTED : updated values should persist after reboot");
	    LOGGER.info("**********************************************************************************");

	    /*
	     * rebooting the device to verify the persistence of
	     * Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram as false. So, the following steps will
	     * verify result when UpdateNvram is false. Also the updated values (in the previous step) should persist.
	     */

	    if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
		errorMessage = "Failed to verify  the webpa parameter value of UpdateNvram";
		if (BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_SYSCFG_UPDATE_NVRAM, BroadBandTestConstants.FALSE,
			BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {

		    /* Verify whether /nvram/syscfg.db is not available when UpdateNvram is false */
		    errorMessage = "Failed to verify the syscfg.db file in the /nvram, /tmp, /opt/secure/data folder.";
		    if (!CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOG_FILE_SYSCFG)
			    && CommonUtils.isFileExists(device, tapEnv,
				    BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG)
			    && CommonUtils.isFileExists(device, tapEnv,
				    BroadBandCommandConstants.LOG_FILE_TMP_SYSCFG)) {

			errorMessage = "Failed to verify log message "
				+ BroadBandTraceConstants.LOG_MESSAGE_SYSCFG_STORED_IN_OPT_SECURE_DATA;
			if (CommonMethods.isNotNull(CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
				BroadBandTraceConstants.LOG_MESSAGE_SYSCFG_STORED_IN_OPT_SECURE_DATA,
				(CommonMethods.isAtomSyncAvailable(device, tapEnv)
					? BroadBandCommandConstants.FILE_ARMCONSOLELOG
					: BroadBandCommandConstants.FILE_CONSOLELOG),
				BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {

			    /* Verify the updated values (in the previous step) should persist */

			    status = verifyUupdatedValuesPersistInSyscfgDbFile(device, tapEnv,
				    cmdSyscfgShowSnmpv3Support, BroadBandTestConstants.TRUE.toLowerCase())
				    && verifyUupdatedValuesPersistInSyscfgDbFile(device, tapEnv,
					    cmdSyscfgShowFirewallLevelv6,
					    BroadBandTestConstants.STRING_FIREWALL_LEVEL_V6)
				    && verifyUupdatedValuesPersistInSyscfgDbFile(device, tapEnv,
					    cmdSyscfgShowUpdateNvram, BroadBandTestConstants.FALSE.toLowerCase());
			}
		    }
		}
	    }

	    if (status)

	    {
		LOGGER.info("STEP 11: ACTUAL : updated values persist after reboot");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "default values are not obtained after removing /opt/secure/data/syscfg.db";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify whether default values are obtained after removing /opt/secure/data/syscfg.db");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute the following command: 	1. rm -rf /opt/secure/data/syscfg.db	2. reboot	 3. curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA URL>/api/v2/device/mac:<ECM_MAC>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram	4. ls -ltr /tmp/syscfg.db  /opt/secure/data/syscfg.db /nvram/syscfg.db	5. 5. grep -i \"Syscfg stored in /nvram/syscfg.db\" /rdklogs/logs/Consolelog.txt.0 	6. syscfg show | grep -i UPdateNvram");
	    LOGGER.info(
		    "STEP 12: EXPECTED : default values should be obtained after removing /opt/secure/data/syscfg.db");
	    LOGGER.info("**********************************************************************************");

	    /*
	     * removing the file /opt/secure/data/syscfg.db and rebooting the device, will set
	     * Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SysCfg.UpdateNvram to true. So, the following steps will
	     * verify result when UpdateNvram is true (Default state)
	     */

	    if (CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG)) {

		if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {

		    /* Verify whether webpa parameter webpa parameter UpdateNvram is true */
		    errorMessage = "Failed to verify the value of webpa parameter "
			    + BroadBandWebPaConstants.WEBPA_PARAM_SYSCFG_UPDATE_NVRAM;
		    if (BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_SYSCFG_UPDATE_NVRAM, BroadBandTestConstants.TRUE,
			    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {

			/* Verify whether /nvram/syscfg.db is available when webpa parameter UpdateNvram is true */
			errorMessage = "Failed to verify the syscfg.db file in the /nvram, /tmp, /opt/secure/data folder.";
			if (CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOG_FILE_SYSCFG)
				&& CommonUtils.isFileExists(device, tapEnv,
					BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG)
				&& CommonUtils.isFileExists(device, tapEnv,
					BroadBandCommandConstants.LOG_FILE_TMP_SYSCFG)) {

			    errorMessage = "Failed to verify log message "
				    + BroadBandTraceConstants.LOG_MESSAGE_SYSCFG_STORED_IN_NVRAM;
			    if (CommonMethods.isNotNull(CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
				    BroadBandTraceConstants.LOG_MESSAGE_SYSCFG_STORED_IN_NVRAM,
				    (CommonMethods.isAtomSyncAvailable(device, tapEnv)
					    ? BroadBandCommandConstants.FILE_ARMCONSOLELOG
					    : BroadBandCommandConstants.FILE_CONSOLELOG),
				    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
				    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
				LOGGER.info(
					"\"Syscfg stored in /nvram/syscfg.db\" log message is available in Consolelog.txt.0 or ArmConsolelog.txt.0");

				/*
				 * Verify whether syscfg value of UPdateNvram is obtained as true when webpa parameter
				 * UpdateNvram is true
				 */
				status = verifyUupdatedValuesPersistInSyscfgDbFile(device, tapEnv,
					cmdSyscfgShowUpdateNvram, BroadBandTestConstants.TRUE.toLowerCase());

			    }
			}
		    }
		}
	    }

	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : default values are obtained after removing /opt/secure/data/syscfg.db");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSCFG-1000");
    }

    /*
     * This method compares syscfg show output with the content of syscfg.db file.
     * 
     * @param device instance of {@link Dut}
     * 
     * @param tapApi instance of {@link AutomaticsTapApi}
     * 
     * @param syscfgShowCommand syscfg show command
     * 
     * @param contentOfSyscfgDb content of syscfg.db file
     * 
     * @return comparisonStatus true if the syscfg show output is same as the content of syscfg.db file else false
     * 
     * @author Dipankar Nalui
     * 
     * @refactor Said Hisham
     * 
     */

    public boolean compareSyscfgShowOutputWithTheContentOfSyscfgDbFile(Dut device, AutomaticsTapApi tapEnv,
	    String syscfgShowCommand, String contentOfSyscfgDb) {
	boolean comparisonStatus = false;
	String output = null;
	String response = null;
	output = tapEnv.executeCommandUsingSsh(device, syscfgShowCommand);
	if (CommonMethods.isNotNull(output)) {
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, contentOfSyscfgDb,
		    BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    comparisonStatus = CommonMethods.isNotNull(response) && output.trim().equalsIgnoreCase(response.trim());
	}
	return comparisonStatus;
    }

    /*
     * This method is to verify the updated values should persist in syscfg.db file
     * 
     * @param device instance of {@link Dut}
     * 
     * @param tapApi instance of {@link AutomaticsTapApi}
     * 
     * @param syscfgShowCommand syscfg show command
     * 
     * @param updatedValue updated value in the previous step
     * 
     * @return persistStatus true if the syscfg show output is same as the content of syscfg.db file else false
     * 
     * @author Dipankar Nalui
     * 
     * @refactor Said Hisham
     */

    public boolean verifyUupdatedValuesPersistInSyscfgDbFile(Dut device, AutomaticsTapApi tapEnv,
	    String syscfgShowCommand, String updatedValue) {
	boolean persistStatus = false;
	String response = null;
	response = tapEnv.executeCommandUsingSsh(device, syscfgShowCommand);
	persistStatus = CommonMethods.isNotNull(response) && CommonUtils
		.isGivenStringAvailableInCommandOutput(response.trim().toLowerCase(), updatedValue.toLowerCase());
	return persistStatus;
    }

    /**
     * Verify the sensitive keywords are removed from iptables
     * <ol>
     * <li>Verify the data in the iptable for IPv4 interface can be retrieved from the gateway</li>
     * <li>Verify the \"configparam\" keyword is not present in iptable</li>
     * <li>Verify the \"whitelist\" keyword is not present in iptable</li>
     * <li>Verify the \"firewall\" keyword is not present in iptable</li>
     * <li>Verify the \"encrypt\" keyword is not present in iptable</li>
     * <li>Verify the \"decrypt\" keyword is not present in iptable</li>
     * <li>Verify the \"secret\" keyword is not present in iptable</li>
     * <li>Verify the \"private\" keyword is not present in iptable</li>
     * <li>Verify the \"shared\" keyword is not present in iptable</li>
     * <li>Verify the \"PSK\" keyword is not present in iptable</li>
     * <li>Verify the \"password\" keyword is not present in iptable</li>
     * <li>Verify the \"credential\" keyword is not present in iptable</li>
     * <li>Verify the \"key\" keyword is not present in iptable</li>
     * <li>Verify the \"dropbear\" keyword is not present in iptable</li>
     * <li>Verify the \"passphrase\" keyword is not present in iptable</li>
     * <li>Verify the \"obfuscate\" keyword is not present in iptable</li>
     * <li>Verify the \"PROPRIETARY\" keyword is not present in iptable</li>
     * </ol>
     * 
     * @param Dut
     * 
     * @author Sathyakishore.N
     * @refactor Said Hisham
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-PRO-INFO-5001")
    public void testToVerifySensitiveKeywordsAreRemovedFromIptable(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-PRO-INFO-501";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-PRO-INFO-5001");
	LOGGER.info("TEST DESCRIPTION: Verify the sensitive keywords are removed from iptables");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the data in the iptable for IPv4 interface can be retrieved from the gateway");
	LOGGER.info("2. Verify the \"configparam\" keyword is not present in iptable");
	LOGGER.info("3. Verify the \"whitelist\" keyword is not present in iptable");
	LOGGER.info("4. Verify the \"firewall\" keyword is not present in iptable");
	LOGGER.info("5. Verify the \"encrypt\" keyword is not present in iptable");
	LOGGER.info("6. Verify the \"decrypt\" keyword is not present in iptable");
	LOGGER.info("7. Verify the \"secret\" keyword is not present in iptable");
	LOGGER.info("8. Verify the \"private\" keyword is not present in iptable");
	LOGGER.info("9. Verify the \"shared\" keyword is not present in iptable");
	LOGGER.info("10. Verify the \"PSK\" keyword is not present in iptable");
	LOGGER.info("11. Verify the \"password\" keyword is not present in iptable");
	LOGGER.info("12. Verify the \"credential\" keyword is not present in iptable");
	LOGGER.info("13. Verify the \"key\" keyword is not present in iptable");
	LOGGER.info("14. Verify the \"dropbear\" keyword is not present in iptable");
	LOGGER.info("15. Verify the \"passphrase\" keyword is not present in iptable");
	LOGGER.info("16. Verify the \"obfuscate\" keyword is not present in iptable");
	LOGGER.info("17. Verify the \"PROPRIETARY\" keyword is not present in iptable");

	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "s1";
	    errorMessage = "Unable to retrieve data in the iptable for IPv4 interface from the gateway";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify the data in the iptable for IPv4 interface can be retrieved from the gateway");
	    LOGGER.info("STEP 1: ACTION : Execute the command in the device console : iptables-save");
	    LOGGER.info("STEP 1: EXPECTED : Data from IPv4 iptable should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");
	    String iptableResponse = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_TO_GET_IPTABLE);
	    status = CommonMethods.isNotNull(iptableResponse)
		    && CommonUtils.patternSearchFromTargetString(iptableResponse, "iptables-save");
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Data in the iptable for IPv4 interface is retrieved successfully from the gateway");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    checkSensitiveKeywords(device, testCaseId, BroadBandTestConstants.CONSTANT_2, iptableResponse, "iptables",
		    BroadBandTestConstants.SENSITIVE_KEYWORDS_REMOVED_FROM_IPTABLE);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-PRO-INFO-5001");
    }

    public void checkSensitiveKeywords(Dut device, String testCaseId, int stepNo, String iptableResponse,
	    String iptableInterface, List<String> listOfKeywords) throws Exception {

	String stepNum = "";
	String errorMessage = "";
	boolean status = false;

	try {

	    for (String keyword : listOfKeywords) {

		stepNum = "s" + stepNo;
		errorMessage = "The sensitive keyword '" + keyword + "' is not removed from " + iptableInterface;
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNo + ": DESCRIPTION : Verify the '" + keyword + "' keyword is not present in "
			+ iptableInterface);
		LOGGER.info(
			"STEP " + stepNo + ": ACTION : Search for keyword '" + keyword + "' in " + iptableInterface);
		LOGGER.info("STEP " + stepNo + ": EXPECTED : The keyword '" + keyword + "' should not present in "
			+ iptableInterface);
		LOGGER.info("**********************************************************************************");
		status = !CommonUtils.patternSearchFromTargetString(iptableResponse, keyword);
		if (status) {
		    LOGGER.info("STEP " + stepNo + ": ACTUAL : The sensitive keyword '" + keyword + "' is removed from "
			    + iptableInterface);
		} else {
		    LOGGER.error("STEP " + stepNo + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		stepNo++;

	    }
	} catch (Exception e) {
	    throw e;
	}
    }
    
    
    /**
     * Verify the sensitive keywords are removed from ip6tables
     * 
     * <ol>
     * <li>Verify the data in the ip6table for IPv4 interface can be retrieved from the gateway</li>
     * <li>Verify the \"configparam\" keyword is not present in ip6table</li>
     * <li>Verify the \"whitelist\" keyword is not present in ip6table</li>
     * <li>Verify the \"firewall\" keyword is not present in ip6table</li>
     * <li>Verify the \"encrypt\" keyword is not present in ip6table</li>
     * <li>Verify the \"decrypt\" keyword is not present in ip6table</li>
     * <li>Verify the \"secret\" keyword is not present in ip6table</li>
     * <li>Verify the \"private\" keyword is not present in ip6table</li>
     * <li>Verify the \"shared\" keyword is not present in ip6table</li>
     * <li>Verify the \"PSK\" keyword is not present in ip6table</li>
     * <li>Verify the \"password\" keyword is not present in ip6table</li>
     * <li>Verify the \"credential\" keyword is not present in ip6table</li>
     * <li>Verify the \"key\" keyword is not present in ip6table</li>
     * <li>Verify the \"dropbear\" keyword is not present in ip6table</li>
     * <li>Verify the \"passphrase\" keyword is not present in ip6table</li>
     * <li>Verify the \"obfuscate\" keyword is not present in ip6table</li>
     * <li>Verify the \"PROPRIETARY\" keyword is not present in ip6table</li>
     * </ol>
     * 
     * @param Dut
     * 
     * @author Sathyakishore.N
     * @refactor Rakesh C N
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-PRO-INFO-5002")
    public void testToVerifySensitiveKeywordsAreRemovedFromIp6table(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-PRO-INFO-502";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-PRO-INFO-5001");
	LOGGER.info("TEST DESCRIPTION: Verify the sensitive keywords are removed from ip6tables");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the data in the ip6table for IPv4 interface can be retrieved from the gateway");
	LOGGER.info("2. Verify the \"configparam\" keyword is not present in ip6table");
	LOGGER.info("3. Verify the \"whitelist\" keyword is not present in ip6table");
	LOGGER.info("4. Verify the \"firewall\" keyword is not present in ip6table");
	LOGGER.info("5. Verify the \"encrypt\" keyword is not present in ip6table");
	LOGGER.info("6. Verify the \"decrypt\" keyword is not present in ip6table");
	LOGGER.info("7. Verify the \"secret\" keyword is not present in ip6table");
	LOGGER.info("8. Verify the \"private\" keyword is not present in ip6table");
	LOGGER.info("9. Verify the \"shared\" keyword is not present in ip6table");
	LOGGER.info("10. Verify the \"PSK\" keyword is not present in ip6table");
	LOGGER.info("11. Verify the \"password\" keyword is not present in ip6table");
	LOGGER.info("12. Verify the \"credential\" keyword is not present in ip6table");
	LOGGER.info("13. Verify the \"key\" keyword is not present in ip6table");
	LOGGER.info("14. Verify the \"dropbear\" keyword is not present in ip6table");
	LOGGER.info("15. Verify the \"passphrase\" keyword is not present in ip6table");
	LOGGER.info("16. Verify the \"obfuscate\" keyword is not present in ip6table");
	LOGGER.info("17. Verify the \"PROPRIETARY\" keyword is not present in ip6table");

	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "s1";
	    errorMessage = "Unable to retrieve data in the ip6table for IPv6 interface from the gateway";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify the data in the ip6table for IPv6 interface can be retrieved from the gateway");
	    LOGGER.info("STEP 1: ACTION : Execute the command in the device console : ip6tables-save");
	    LOGGER.info("STEP 1: EXPECTED : Data from IPv6 ip6table should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");
	    String iptableResponse = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_TO_GET_IPV6TABLE);
	    status = CommonMethods.isNotNull(iptableResponse)
		    && CommonUtils.patternSearchFromTargetString(iptableResponse, "ip6tables-save");
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Data in the iptable for IPv6 interface is retrieved successfully from the gateway");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    checkSensitiveKeywords(device, testCaseId, BroadBandTestConstants.CONSTANT_2, iptableResponse, "ip6tables",
		    BroadBandTestConstants.SENSITIVE_KEYWORDS_REMOVED_FROM_IPTABLE);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-PRO-INFO-5002");
    }
    
}
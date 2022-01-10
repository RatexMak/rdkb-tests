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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.CommonUtils;
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
	    
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv) ) {
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
	    }  else {
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
     *  Verify DBus -Overly permissive system setting.
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
}
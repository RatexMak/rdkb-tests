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
package com.automatics.rdkb.tests.wifi;

/**
 * Test class with test case for validating TR-181 Parameters using WebPA Protocol
 * 
 * @author Prabhakaran ,Joseph_Maduram
 * @Refactor Athira ,Alan_Bivera
 */
import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWiFiWebPaTest extends AutomaticsTestBase {

    /**
     * This test case is to check the Security modes on 5.0Ghz Private Network
     * 
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10101.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     *
     * @Refactor Athira
     * @param device
     *            device to be used
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1005", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10101.Security.ModeEnabled using WebPA ")
    public void testVerifyAllSupportedSecurityModeFor5GhzPrivateNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-105";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED);
    }

    /**
     * This test case is to check the Security mode on 5.0Ghz Network
     * 
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10102.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     *
     * @Refactor Athira
     * @param device
     *            device to be used
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1006", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10102.Security.ModeEnabled using WebPA ")
    public void testVerifyAllSupportedSecurityModeFor5GhzNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-106";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_SECURITY_MODEENABLED);
    }

    /**
     * 
     * Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10103.Security.ModeEnabled
     * 
     * This test case is to check the Security mode on 5.0Ghz Public Network
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10103.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     * 
     * @Refactor Alan_Bivera
     * @param device
     *            Device to be used
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1007", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10103.Security.ModeEnabled using WebPA")
    public void testVerifyAllSupportedSecurityModeFor5GhzPublicNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-107";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PUBLIC_SECURITY_MODEENABLED);
    }

    /**
     * 
     * Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10001.Security.ModeEnabled
     * 
     * This test case is to check the Security mode on 2.4Ghz Private Network
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10001.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     * 
     * @Refactor Alan_Bivera
     * @param device
     *            Device to be used
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1008", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10001.Security.ModeEnabled using WebPA")
    public void testVerifyAllSupportedSecurityModeFor2_4GhzPrivateNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-108";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED);
    }

    /**
     * 
     * Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10002.Security.ModeEnabled
     * 
     * This test case is to check the Security mode on 2.4Ghz Network
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10002.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     * 
     * @Refactor Alan_Bivera
     * @param device
     *            Device to be used
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1009", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10002.Security.ModeEnabled using WebPA")
    public void testVerifyAllSupportedSecurityModeFor2_4GhzNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-109";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_SECURITY_MODEENABLED);
    }

    /**
     * 
     * Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10003.Security.ModeEnabled
     * 
     * This test case is to check the Security mode on 2.4Ghz Public Network
     * <ol>
     * <li>STEP 1:Execute webPA Get command on Device.WiFi.AccessPoint.10003.Security.ModeEnabled to get the current
     * mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to Set on all the valid security mode parameter and validate whether they are
     * set correctly</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change</li>
     * 
     * <li>STEP 4 :Execute WebPA command to Set the current security mode and validate whether they are set
     * correctly</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     * 
     * @Refactor Alan_Bivera
     * @param device
     *            Device to be used
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-1010", testDecription = "Verifying the TR-181 Parameter Device.WiFi.AccessPoint.10003.Security.ModeEnabled using WebPA")
    public void testVerifyAllSupportedSecurityModeFor2_4GhzPublicNetwork(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WIFI-WEBPA-110";
	checkForSecurityModeParameter(device, testId,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PUBLIC_SECURITY_MODEENABLED);
    }

    /**
     * To validate all the Security modes
     * 
     * <ol>
     * <li>STEP 1:Execute webPA command to Get on the Security Mode to get the current mode</li>
     * 
     * <li>STEP 2:Execute WebPA command to set Security Mode parameter</li>
     * 
     * <li>STEP 3:Verify whether device logs the telemetry marker for security mode change.</li>
     * 
     * <li>STEP 4 :Execute WebPA command to set the current security mode parameter</li>
     * 
     * <li>STEP 5:Verify whether device logs the telemetry marker for current security mode change</li>
     * </ol>
     * 
     * @param device
     *            Device to be used
     * @Refactor Athira
     */

    public void checkForSecurityModeParameter(Dut device, String testCaseId, String securityModeEnabled) {
	// String to store the test step number
	String testStepNumber = null;
	// String to store the test case ID
	String testId = testCaseId;
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// String to store the Current security mode
	String currentSecurityMode = "";
	// list of strings to store the supported security modes
	List<String> supportedSecurityModesList = new ArrayList<String>();
	supportedSecurityModesList.add(BroadBandTestConstants.SECURITY_MODE_NONE);
	supportedSecurityModesList.add(BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
	supportedSecurityModesList.add(BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE);

	// stores the step number
	int stepNumber = 0;
	try {

	    /**
	     * STEP 1:Execute webPA command to Get on the Security Mode to get the current mode
	     */
	    testStepNumber = "s1";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Execute  webPA Get command on the Security ModeEnabled parameter to get the current mode");
	    LOGGER.info("EXPECTED: Should retrieve the current security modes in the device");
	    LOGGER.info("#####################################################################################");
	    currentSecurityMode = tapEnv.executeWebPaCommand(device, securityModeEnabled);
	    LOGGER.info("CURRENT SECURITY MODE SET IN THE DEVICE " + "=" + currentSecurityMode);
	    status = CommonMethods.isNotNull(currentSecurityMode);
	    errorMessage = "webPA Get on Security Mode parmater gives empty or null response for WebPA command";
	    LOGGER.info(
		    "S1 ACTUAL : " + (status ? "webPA Get on Security Mode parameters to get the current mode - SUCCESS"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 2 : Execute WebPA command to set Security Mode parameter
	     */
	    for (int count = 0; count < supportedSecurityModesList.size(); count++) {

		if (count == 0) {
		    stepNumber = 2 + count;
		}
		if (currentSecurityMode.equalsIgnoreCase(supportedSecurityModesList.get(count))) {
		    continue;
		}
		testStepNumber = "s" + (stepNumber);
		status = false;
		LOGGER.info("#####################################################################################");
		LOGGER.info(testStepNumber + " " + ":Execute WebPA command to set Security Mode parameter-"
			+ securityModeEnabled + "to value as " + "-" + supportedSecurityModesList.get(count));
		LOGGER.info("EXPECTED: Device Should be able to set the security mode to"
			+ supportedSecurityModesList.get(count) + "for security mode parameter-" + securityModeEnabled);
		LOGGER.info("#####################################################################################");
		String timeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv, device);
		LOGGER.info("######timeStamp is###########" + timeStamp);
		status = BroadBandWebPaUtils.setAndVerifyAllSecurityModesSupported(device, tapEnv, securityModeEnabled,
			supportedSecurityModesList.get(count));
		stepNumber++;
		errorMessage = "WebPA Set on the security mode parameter got failed";
		;
		LOGGER.info(testStepNumber + " " + "ACTUAL : "
			+ (status
				? "webPA Set on Security ModeEnabled parameter as" + " "
					+ supportedSecurityModesList.get(count) + "-SUCCESS"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		/**
		 * STEP 3 : Verify whether device logs the telemetry marker for security mode change.
		 * 
		 */
		testStepNumber = "s" + stepNumber;
		status = false;
		LOGGER.info("#####################################################################################");
		LOGGER.info(testStepNumber + " "
			+ "Verify whether device logs the telemetry marker for security mode change to-"
			+ supportedSecurityModesList.get(count));
		LOGGER.info("Device should logs the telemetry marker for security mode change.");
		LOGGER.info("#####################################################################################");
		status = BroadBandWebPaUtils.checkingForTelemetryMarkerInDeviceAfterSettingSecurityModes(device, tapEnv,
			supportedSecurityModesList.get(count), timeStamp);
		stepNumber++;
		errorMessage = "Appropriate Telemetry marker is not logged when Secuity mode is set to "
			+ supportedSecurityModesList.get(count);

		LOGGER.info(testStepNumber + " " + " ACTUAL : "
			+ (status
				? "Appropriate Telemetry marker is logged when Secuity mode is set to " + " "
					+ supportedSecurityModesList.get(count) + " - SUCCESS"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	    }

	    /**
	     * STEP 4 :Execute WebPA command to set the current security mode parameter
	     * 
	     */
	    testStepNumber = "s" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(testStepNumber + " " + ": Execute WebPA command to set security mode parameter - "
		    + securityModeEnabled + " with value of current security mode as" + "-" + currentSecurityMode);
	    LOGGER.info("EXPECTED: Should be able to set the current security mode to " + currentSecurityMode
		    + " for security mode parameter - " + securityModeEnabled);
	    LOGGER.info("#####################################################################################");
	    String timeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv, device);
	    LOGGER.info("######timeStamp is###########" + timeStamp);
	    stepNumber++;
	    status = BroadBandWebPaUtils.setAndVerifyAllSecurityModesSupported(device, tapEnv, securityModeEnabled,
		    currentSecurityMode);

	    errorMessage = "WebPA Set on the current modeEnabled parameter got failed";
	    LOGGER.info(testStepNumber + " " + "ACTUAL : "
		    + (status
			    ? "webPA Set on current Security ModeEnabled parameter as " + currentSecurityMode
				    + "-SUCCESS"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 5: Verify whether device logs the telemetry marker for current security mode change
	     * 
	     */
	    testStepNumber = "s" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(testStepNumber + " "
		    + "Verify whether device logs the telemetry marker for current security mode change to-"
		    + currentSecurityMode);
	    LOGGER.info("EXPECTED: Device should log the telemetry marker for current security mode change");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.checkingForTelemetryMarkerInDeviceAfterSettingSecurityModes(device, tapEnv,
		    currentSecurityMode, timeStamp);

	    errorMessage = "Appropriate Telemetry marker is not logged when Secuity mode is set to "
		    + currentSecurityMode;

	    LOGGER.info(testStepNumber + " " + " ACTUAL : "
		    + (status
			    ? "Appropriate Telemetry marker is logged when Secuity mode is set to" + " "
				    + currentSecurityMode + " - SUCCESS"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception exception) {

	    LOGGER.error("Exception occured in verifying Security.ModeEnabled parameters" + exception.getMessage());
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	}

    }
}

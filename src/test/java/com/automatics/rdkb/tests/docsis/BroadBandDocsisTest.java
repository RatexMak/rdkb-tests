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
package com.automatics.rdkb.tests.docsis;

import java.util.ArrayList;
import java.util.Iterator;

import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants.BootStrapParametersRemoveList;
import com.automatics.rdkb.constants.BroadBandWebPaConstants.BootStrapParametersRfcList;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandDocsisTest extends AutomaticsTestBase {
    /**
     * Test to validate the timestamp for upstream/downstream ranging
     * 
     * <li>1. Perform factory reset on the unit via WebPa</li>
     * <li>2. Validate if /rdklogs/logs/CMlog.txt.0 file is present</li>
     * <li>3. Get the expected response for downstream and upstream from CMlog.txt.0</li>
     * 
     * @param device
     * 
     * @author Sathurya_R
     * @refactor Govardhan
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-DOWNUPSTREAM-1001")
    public void testVerifyTimestampUpstreamDownstream(Dut device) {

	// Variable declaration begins
	String testCaseId = "";
	String stepNum = "S1";
	String errorMessage = "";
	boolean status = false;
	String response = null;
	// Variable declaration ends

	testCaseId = "TC-RDKB-DOWNUPSTREAM-001";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-DOWNUPSTREAM-1001");
	LOGGER.info("TEST DESCRIPTION: Validate the timestamp for upstream/downstream ranging ");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Perform factory reset on the unit via WebPa ");
	LOGGER.info("2. Validate if /rdklogs/logs/CMlog.txt.0 file is present ");
	LOGGER.info("3. Get the expected response for downstream and upstream from CMlog.txt.0 ");

	LOGGER.info("#######################################################################################");

	try {

	    errorMessage = "Attempt to perform factory reset via webpa has failed";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 1: DESCRIPTION : Perform factory reset on the unit via WebPa ");
	    LOGGER.info("STEP 1: ACTION : a) Execute WebPa SET command on the object "
		    + "Device.X_CISCO_COM_DeviceControl.FactoryReset using the command, "
		    + "\"curl --request GET --url '"+BroadbandPropertyFileHandler.getWebpaServerURL()+"<ECM MAC>/config?" + "names=Device.X_CISCO_COM_DeviceControl.FactoryReset' "
		    + "--header 'authorization: Bearer <SAT-TOKEN>'\"");
	    LOGGER.info(
		    "STEP 1: EXPECTED : The parameter should get set successfully and return a 200 success response");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);

	    if (status) {
		LOGGER.info("STEP 1 ACTUAL : Attempt to factory reset via WEBPA is successfull!");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "The log file /rdklogs/logs/CMlog.txt.0 is not present even after 10 minutes of waitime";
	    status = false;

	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 2: DESCRIPTION : Validate if /rdklogs/logs/CMlog.txt.0 file is present ");
	    LOGGER.info("STEP 2: ACTION : execute the following command inside the RG console of the gateway, "
		    + "\"if [ -f /rdklogs/logs/CMLog.txt.0 ] ; then echo \"true\" ; else echo \"false\" ; fi\"");
	    LOGGER.info("STEP 2: EXPECTED : The file /rdklogs/logs/CMLog.txt.0 parameter should present ");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandCommonUtils.doesFileExistWithinGivenTimeFrameInArm(tapEnv, device,
		    BroadBandTestConstants.CM_LOG_FILE_NAME, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : The log file /rdklogs/logs/CMLog.txt.0 is present on the device");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S3";
	    errorMessage = "The expected logs are not coming up in /rdklogs/logs/CMLog.txt.0 ";
	    status = false;
	    long startTimeStamp = System.currentTimeMillis();

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Get the expected response for downstream and upstream from CMlog.txt.0 ");
	    LOGGER.info("STEP 3: ACTION : execute the following command inside the RG console of the gateway, "
		    + "\"cat /rdklogs/logs/CMlog.txt.0 |grep -E \"Downstream|Upstream\"");
	    LOGGER.info("STEP 3: EXPECTED : The expected logs should be present ");
	    LOGGER.info("#####################################################################################");

	    do {

		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CMD_TO_GET_UPSTREAM_DOWNSTREAM_LOG);
		LOGGER.info("Response is : "+response);
		status = CommonMethods.isNotNull(response)
			&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_MATCH_DOWNSTREAM_LOCK)
			&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_MATCH_UPSTREAM_LOCK);

	    } while (!status
		    && (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
		    && CommonMethods.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : The expected downstream/upstream logs are present in /rdklogs/logs/CMlog.txt.0");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	} catch (Exception exeception) {
	    errorMessage = "Exception occurred while validating the the timestamp for upstream/downstream ranging"
		    + exeception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    /**
	     * POST-CONDITION 1 : ENABLE THE XIFNITY WIFI
	     */
	    BroadBandPostConditionUtils.executePostConditionToEnablePublicWifi(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);

	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}

    }
    
    /**
     * <li>1. Get partner id using webpa operation</li>
     * <li>2. Verify bootstrap.json file present under /nvram</li>
     * <li>3. Get TR181 parameters and active values from bootstrap.json file</li>
     * <li>4. Configure new values for bootstrap parameters using RFC</li>
     * <li>5. Verify bootstrap configuration updated through RFC in dcmrfc.log file</li>
     * <li>6. Verify bootstrap version in bootstrap.json file</li>
     * <li>7. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation</li>
     * <li>8. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation</li>
     * <li>9. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa
     * operation</li>
     * <li>10. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation</li>
     * <li>11. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa
     * operation</li>
     * <li>12. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation</li>
     * <li>13. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation</li>
     * <li>14. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation</li>
     * <li>15. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation</li>
     * <li>16. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using
     * webpa operation</li>
     * <li>17. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa
     * operation</li>
     * <li>18. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage
     * using webpa operation</li>
     * <li>19. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using
     * webpa operation</li>
     * <li>20. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using dmcli operation</li>
     * <li>21. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using dmcli operation</li>
     * <li>22. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using dmcli
     * operation</li>
     * <li>23. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using dmcli operation</li>
     * <li>24. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using dmcli
     * operation</li>
     * <li>25. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using dmcli operation</li>
     * <li>26. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using dmcli operation</li>
     * <li>27. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using dmcli operation</li>
     * <li>28. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using dmcli operation</li>
     * <li>29. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using
     * dmcli operation</li>
     * <li>30. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using dmcli
     * operation</li>
     * <li>31. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage
     * using dmcli operation</li>
     * <li>32. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using
     * dmcli operation</li>
     * <li>33. Update the bootstrap version & read only parameters with different value & update source as - for
     * all the parameters in bootstap.json file</li>
     * <li>34. Reboot the device</li>
     * <li>35. Verify bootstrap version,response should contain the latest version</li>
     * <li>36. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation</li>
     * <li>37. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation</li>
     * <li>38. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa
     * operation</li>
     * <li>39. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation</li>
     * <li>40. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa
     * operation</li>
     * <li>41. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation</li>
     * <li>42. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation</li>
     * <li>43. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation</li>
     * <li>44. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation</li>
     * <li>45. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using
     * webpa operation</li>
     * <li>46. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa
     * operation</li>
     * <li>47. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage
     * using webpa operation</li>
     * <li>48. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using
     * webpa operation</li>
     * 
     * @author ArunKumar Jayachandran
     * @refactor yamini.s
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.SECURITY })
    @TestDetails(testUID = "TC-RDKB-BOOTSTRAP-1031")
    public void testToVerifyBootStrapConfigWithManualOverride(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-BOOTSTRAP-1031");
	LOGGER.info("TEST DESCRIPTION: Test to verify bootstrap configuration");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Get partner id using webpa operation");
	LOGGER.info("2. Verify bootstrap.json file present under /nvram");
	LOGGER.info("3. Get TR181 parameters and active values from bootstrap.json file");
	LOGGER.info("4. Configure new values for bootstrap parameters using RFC");
	LOGGER.info("5. Verify bootstrap configuration updated through RFC in dcmrfc.log file");
	LOGGER.info("6. Verify bootstrap version in bootstrap.json file");
	LOGGER.info(
		"7. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation");
	LOGGER.info(
		"8. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation");
	LOGGER.info(
		"9. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa operation");
	LOGGER.info(
		"10. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation");
	LOGGER.info(
		"11. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa operation");
	LOGGER.info(
		"12. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation");
	LOGGER.info(
		"13. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation");
	LOGGER.info(
		"14. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation");
	LOGGER.info(
		"15. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation");
	LOGGER.info(
		"16. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using webpa operation");
	LOGGER.info(
		"17. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa operation");
	LOGGER.info(
		"18. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using webpa operation");
	LOGGER.info(
		"19. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using webpa operation");
	LOGGER.info(
		"20. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using dmcli operation");
	LOGGER.info(
		"21. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using dmcli operation");
	LOGGER.info(
		"22. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using dmcli operation");
	LOGGER.info(
		"23. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using dmcli operation");
	LOGGER.info(
		"24. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using dmcli operation");
	LOGGER.info(
		"25. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using dmcli operation");
	LOGGER.info(
		"26. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using dmcli operation");
	LOGGER.info(
		"27. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using dmcli operation");
	LOGGER.info(
		"28. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using dmcli operation");
	LOGGER.info(
		"29. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using dmcli operation");
	LOGGER.info(
		"30. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using dmcli operation");
	LOGGER.info(
		"31. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using dmcli operation");
	LOGGER.info(
		"32. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using dmcli operation");
	LOGGER.info(
		"33. Update the bootstrap version & read only parameters with different value & update source as - for all the parameters in bootstap.json file");
	LOGGER.info("34. Reboot the device");
	LOGGER.info("35. Verify bootstrap version,response should contain the latest version");
	LOGGER.info(
		"36. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation");
	LOGGER.info(
		"37. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation");
	LOGGER.info(
		"38. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa operation");
	LOGGER.info(
		"39. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation");
	LOGGER.info(
		"40. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa operation");
	LOGGER.info(
		"41. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation");
	LOGGER.info(
		"42. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation");
	LOGGER.info(
		"43. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation");
	LOGGER.info(
		"44. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation");
	LOGGER.info(
		"45. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using webpa operation");
	LOGGER.info(
		"46. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa operation");
	LOGGER.info(
		"47. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using webpa operation");
	LOGGER.info(
		"48. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using webpa operation");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-BOOTSTRAP-031";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// String to store response
	String response = null;
	String version = null;
	// variable declaration ends
	String logMessagePartner = null;
	ArrayList<String> paramList = new ArrayList<String>();
	ArrayList<String> activeValueList = new ArrayList<String>();
	ArrayList<String> rfcParamList = new ArrayList<String>();
	BootStrapParametersRfcList[] rfcList = BootStrapParametersRfcList.values();

	try {
	    // Get the parameter list from enum
	    for (BootStrapParametersRfcList parameter2 : rfcList) {
		rfcParamList.add(parameter2.getParameterName());
	    }

	    // pre condition, step 1 & 2 to get the partner id & verify bootstrap.json file present in /nvram directory
	    String partnerId = verifyPartnerAndBootstrapJsonFile(device, testCaseId, BroadBandTestConstants.CONSTANT_1,
		    rfcParamList);

	    stepNumber = "s3";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: Get TR181 parameters and active values from bootstrap.json file");
	    LOGGER.info("STEP 3: ACTION: Execute command cat /nvram/bootstrap.json");
	    LOGGER.info("STEP 3: EXPECTED: Should get the parameter and active values from bootstrap.json file");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response from /nvram/bootstap.json file";
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTestConstants.CAT_COMMAND, BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON));
	    if (CommonMethods.isNotNull(response)) {
		activeValueList = GetValuesFromBootStrapJsonFile(response, partnerId,
			BroadBandTestConstants.KEY_ACTIVE_VALUE);
		paramList = GetValuesFromBootStrapJsonFile(response, partnerId,
			BroadBandTestConstants.STRING_PARAMETERS);
	    }
	    status = paramList.size() != BroadBandTestConstants.CONSTANT_0
		    && activeValueList.size() != BroadBandTestConstants.CONSTANT_0
		    && paramList.size() == activeValueList.size();
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL: Successfully got values and parameter list from bootstrap.json file");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    int stepCount = BroadBandTestConstants.CONSTANT_3;
	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Configure new values for bootstrap parameters using RFC");
	    LOGGER.info("STEP " + stepCount + ": ACTION: update the boostrap parameter values using RFC");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Should updated with new parameters");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to update bootstrap parameters using RFC";
	    status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_CONFIG, true);
	    if (status) {
		LOGGER.info(
			"STEP " + stepCount + ": ACTUAL: Successfully updated the boostrap configuration using RFC");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION: Verify bootstrap configuration updated through RFC in dcmrfc.log file");
	    LOGGER.info("STEP " + stepCount + ": ACTION: grep -i bootstrap_config /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Response should contain the log message in dcmrfc.log file");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message in dcmrfc.log file";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_CONFIG, BroadBandCommandConstants.FILE_DCMRFC_LOG,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_CONFIG);
	    if (status) {
		LOGGER.info("STEP " + stepCount
			+ ": ACTUAL: Successfully verified the boostrap configuration log message in dcmrfc.log file");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Verify bootstrap version in bootstrap.json file");
	    LOGGER.info("STEP " + stepCount + ": ACTION: grep -i version /nvram/bootstrap.json");
	    LOGGER.info(
		    "STEP " + stepCount + ": EXPECTED: Response should contain latest version in bootstrap.json file");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the version details from the bootstrap.json file";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTraceConstants.LOG_MESSAGE_VERSION,
		    BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
	    if (CommonMethods.isNotNull(response)) {
		version = CommonMethods.caseInsensitivePatternFinder(response,
			BroadBandTestConstants.PATTERN_BOOTSTRAP_VERSION);
		status = CommonMethods.isNotNull(version)
			&& !version.equalsIgnoreCase(BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_VERSION_3_0);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepCount
			+ ": ACTUAL: Successfully verified the boostrap latest version in bootstrap.json file");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    logMessagePartner = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.COMMA,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, partnerId, BroadBandTestConstants.COMMA,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	    // verfiy bootstrap parameters values using webpa after rfc update & verify log message for each parameter
	    // in PAM log
	    stepCount = verifyRfcValuesUpdateUsingWebpaForBootstrapParameter(device, testCaseId, rfcParamList,
		    activeValueList, paramList, logMessagePartner, stepCount);

	    // verify bootstrap parameters and dmcli response after rfc update
	    stepCount = verifyRfcValuesUpdateUsingDmcliForBootstrapParameter(device, testCaseId, rfcParamList,
		    activeValueList, paramList, stepCount);

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION: Update the bootstrap version & read only parameters with different value & update source as - for all the parameters in bootstap.json file");
	    LOGGER.info("STEP " + stepCount
		    + ": ACTION: Execute command:Download file from autoValut with all the changes in /nvram");
	    LOGGER.info("STEP " + stepCount
		    + ": EXPECTED: Successfully updated the bootstrap values in /nvram/bootstrap.json file");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to download bootstrap config file from AutoVault";
	    if (CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON)) {
		if (CommonUtils.downloadFileUsingAutoVault(device, tapEnv,
			BroadBandCommandConstants.FILE_AUTOVAULT_BOOTSTRAP, BroadBandCommandConstants.MOUNT_NVRAM)) {
		    status = CommonMethods.isFileExists(device, tapEnv,
			    BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON);
		}
	    }
	    if (status) {
		LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully updated bootstrap.json file");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: remove rfc.properties and Reboot the device");
	    LOGGER.info("STEP " + stepCount + ": ACTION: 1. rm /nvram/rfc.properties 2. /bin/reboot");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Should successfully SSH after reboot");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to reboot the device or device is not sshable after reboot";
	    if (CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES)) {
		status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepCount
			+ ": ACTUAL: Successfully rebooted the device and able to SSH after reboot");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Verify bootstrap version");
	    LOGGER.info("STEP " + stepCount + ": ACTION: grep -i version /nvram/bootstrap.json");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Response should contain the latest version");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the version as 4.0 in bootstrap.json file";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTraceConstants.LOG_MESSAGE_VERSION,
		    BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response, version);
	    if (status) {
		LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified bootstrap version as 4.0");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    // verify webpa response & bootstrap parameter response is same
	    stepCount = verifyBootstrapParametersUsingWebpa(device, testCaseId, rfcParamList, activeValueList,
		    paramList, stepCount);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying Bootstrap configuration" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    postConditionForBootStrapConfig(device);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-BOOTSTRAP-1031");
	// ###############################################################//
    }

    /**
     * Method to verify partner id and bootstrap.json file present in /nvram
     * 
     * @param device
     *            device
     * @param testCaseId
     *            test case id
     * @param stepCount
     *            step count
     * @return partner name
     * 
     * @author ArunKumar Jayachandran
     * @refactor yamini.s
     */
    public String verifyPartnerAndBootstrapJsonFile(Dut device, String testCaseId, int stepCount,
	    ArrayList<String> rfcParamList) throws Exception {
	boolean status = false;
	String errorMessage = null;
	String partnerId = null;
	String response = null;
	String successmessage = null;
	try {
	    LOGGER.info("################################# STARTING PRE-CONFIGURATIONS #############################");
	    /**
	     * PRECONDITION 1 : VERIFY AND DELETE RFC PROPERTIES FILE
	     */
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 1: DESCRIPTION: Delete files and reboot the device to bring the configuration to normal state");
	    LOGGER.info("PRE-CONDITION 1: ACTION: Execute command: rm /nvram/rfc.properties and factoryreset");
	    LOGGER.info(
		    "PRE-CONDITION 1: EXPECTED: Should delete files and Device should be SSHable after factoryreset");
	    LOGGER.info("#####################################################################################");
	    status = false;
	    successmessage = "Active Value does not contain _RFC";
	    try {
		response = tapEnv.executeWebPaCommand(device, rfcParamList.get(BroadBandTestConstants.CONSTANT_0));
		if (CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
			BroadBandTraceConstants.LOG_MESSAGE_RFC)) {
		    successmessage = "rfc.properties file not present in nvram";
		    if (CommonUtils.isFileExists(device, tapEnv,
			    BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES)) {
			LOGGER.info("Status of removing " + BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES
				+ "is : " + status);
			successmessage = "Sucessfully deleted rfc.properties file in nvram and factory reset the device";
			errorMessage = "Error in deleting the rfc.properties file and factory reset the device";
			CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
				BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES);
			status = BroadBandCommonUtils.performFactoryResetWebPaByPassingTriggerTime(tapEnv, device,
				BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS)
				&& !CommonUtils.isFileExists(device, tapEnv,
					BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES);
			LOGGER.info("Status of removing " + BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES
				+ "is : " + status);
		    } else {
			status = true;
		    }
		} else {
		    errorMessage = "webpa response for Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink is null";
		    if (!CommonMethods.isNull(response)) {
			status = true;
		    }
		}

	    } catch (Exception e) {
		LOGGER.error("exception occurred while getting rfc list :" + e.getMessage());
	    }

	    if (status) {
		LOGGER.info("PRE-CONDITION 1 : ACTUAL : " + successmessage);
	    } else {
		LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1: FAILED : " + errorMessage);
	    }

	    LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");

	    String stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get partner id using webpa operation");
	    LOGGER.info("STEP " + stepCount
		    + ": ACTION: Execute webpa command for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PartnerId");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Should get the response for partner id");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for webpa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PartnerId";
	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
	    if (CommonMethods.isNotNull(response)) {
		partnerId = BroadBandCommonUtils.getPartnerIdFromBuildAppender(device, tapEnv);
		status = CommonMethods.isNotNull(partnerId) && response.equalsIgnoreCase(partnerId);
		if (!status) {
		    BroadBandWiFiUtils.changePartnerIdAndReactivateTheDevice(device, partnerId);
		    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID, partnerId,
			    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
		}
	    }
	    if (status) {
		LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully got partner id as : " + partnerId);
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Verify bootstrap.json file present under /nvram");
	    LOGGER.info("STEP " + stepCount + ": ACTION: Execute command ls /nvram/bootstrap.json");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Bootstrap.json file should present in /nvram");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Bootstap.json file not present under /nvram";
	    status = CommonMethods.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON);
	    if (status) {
		LOGGER.info("STEP " + stepCount
			+ ": ACTUAL: Successfully verified presence of bootstap.json file under /nvram");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//
	} catch (TestException exception) {
	    throw exception;
	}
	return partnerId;
    }

    /**
     * Method to get the parameter & values from bootstrap json file
     * 
     * @param response
     *            bootstrap json file output
     * @param partnerId
     *            device partner id
     * @param key
     *            based on the key will get parameter name, active value
     * @return list of parameter or active value
     * 
     * @author ArunKumar Jayachandran
     * @refactor yamini.s
     */
    public ArrayList<String> GetValuesFromBootStrapJsonFile(String response, String partnerId, String key) {
	JSONObject objectName = null;
	JSONObject partnerDetail = null;
	JSONObject parameterValue = null;
	String bootStrapParam = null;
	ArrayList<String> removeParamList = new ArrayList<String>();
	ArrayList<String> values = new ArrayList<String>();
	BootStrapParametersRemoveList[] removeList = BootStrapParametersRemoveList.values();
	for (BootStrapParametersRemoveList parameter1 : removeList) {
	    removeParamList.add(parameter1.getParameterName());
	}
	try {
	    objectName = new JSONObject(response);
	    partnerDetail = objectName.getJSONObject(partnerId);
	    Iterator<String> paramKey = partnerDetail.keys();
	    while (paramKey.hasNext()) {
		bootStrapParam = paramKey.next();
		parameterValue = partnerDetail.getJSONObject(bootStrapParam);
		if (!removeParamList.contains(bootStrapParam)
			&& !bootStrapParam.equalsIgnoreCase(BroadBandTestConstants.TR69_ACS_CONNECTION_URL_PARAMETER)) {
		    if (!key.equalsIgnoreCase(BroadBandTestConstants.STRING_PARAMETERS)) {
			values.add(parameterValue.get(key).toString());
			LOGGER.info(key + ": " + parameterValue.get(key).toString());
		    } else {
			values.add(bootStrapParam);
			LOGGER.info("parameter: " + bootStrapParam);
		    }
		}
	    }
	} catch (Exception e) {
	    LOGGER.error("Exception occured during json parsing");
	}
	return values;
    }

    /**
     * Method to verify rfc values are updated in Bootstrap parameters
     * 
     * @param device
     *            device
     * @param testCaseId
     *            test case id
     * @param rfcParamList
     *            list of parameters for rfc config
     * @param activeValueList
     *            list of active values
     * @param paramList
     *            list of parameters in bootstrap file
     * @param logMessagePartner
     *            log message
     * @param stepCount
     *            step count
     * 
     * @return step count
     * 
     * @author ArunKumar Jayachandran
     * @refactor yamini.s
     */
    public int verifyRfcValuesUpdateUsingWebpaForBootstrapParameter(Dut device, String testCaseId,
	    ArrayList<String> rfcParamList, ArrayList<String> activeValueList, ArrayList<String> paramList,
	    String logMessagePartner, int stepCount) {
	String stepNumber = null;
	String errorMessage = null;
	boolean status = false;
	String response = null;
	for (String parameter : rfcParamList) {
	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get response for parameter " + parameter
		    + " using webpa operation");
	    LOGGER.info("STEP " + stepCount + ": ACTION: Execute webpa get operation for parameter: " + parameter);
	    LOGGER.info("STEP " + stepCount
		    + ": EXPECTED: Response should not be same with bootstrap.json file active value & should be updated with RFC value");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for webpa parameter " + parameter;
	    response = tapEnv.executeWebPaCommand(device, parameter);
	    if (CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTraceConstants.LOG_MESSAGE_RFC)) {
		errorMessage = "Bootstrap value and webpa response are same for parameter: " + parameter
			+ " & RFC configuration are not reflected";
		status = !response.equalsIgnoreCase(activeValueList.get(paramList.indexOf(parameter)));
		if (status) {
		    tapEnv.searchAndWaitForTraceFromStart(device, response + logMessagePartner,
			    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		}

	    }
	    if (status) {
		LOGGER.info("STEP " + stepCount
			+ ": ACTUAL: Successfully verified webpa response with bootstrap.json file");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//
	}
	return stepCount;
    }

    /**
     * Method to get the response using dmcli and check rfc update is present
     * 
     * @param device
     *            device
     * @param testCaseId
     *            test case id
     * @param rfcParamList
     *            list of rfc parameters
     * @param activeValueList
     *            list of active values for bootstrap parameters
     * @param paramList
     *            list of parameters for bootstrap parameters
     * @param stepCount
     *            step count
     * @return step count
     * 
     * @author ArunKumar Jayachandran
     * @refactor yamini.s
     */
    public int verifyRfcValuesUpdateUsingDmcliForBootstrapParameter(Dut device, String testCaseId,
	    ArrayList<String> rfcParamList, ArrayList<String> activeValueList, ArrayList<String> paramList,
	    int stepCount) {
	String stepNumber = null;
	String errorMessage = null;
	boolean status = false;
	String response = null;
	for (String parameter : rfcParamList) {
	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get response for parameter " + parameter
		    + " using dmcli operation");
	    LOGGER.info("STEP " + stepCount + ": ACTION: Execute dmcli get operation for parameter: " + parameter);
	    LOGGER.info("STEP " + stepCount
		    + ": EXPECTED: Response should not be same with bootstrap.json file active value & should update with rfc value");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for dmcli parameter " + parameter;
	    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv, parameter);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Bootstrap value and dmcli response are same for parameter: " + parameter
			+ " & RFC configurations are not reflected";
		status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
			BroadBandTraceConstants.LOG_MESSAGE_RFC)
			&& !response.equalsIgnoreCase(activeValueList.get(paramList.indexOf(parameter)));
	    }
	    if (status) {
		LOGGER.info("STEP " + stepCount
			+ ": ACTUAL: Successfully verified dmcli response with bootstrap.json file");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//
	}
	return stepCount;
    }

    /**
     * Method to get the response using webpa & check with bootstrap.json active value
     * 
     * @param device
     *            device
     * @param testCaseId
     *            test case id
     * @param rfcParamList
     *            list of parameters for rfc config
     * @param activeValueList
     *            list of values for bootstrap file
     * @param paramList
     *            list of parameters
     * @param stepCount
     *            step count
     * @return step count
     * 
     * @author ArunKumar Jayachandran
     * @refactor yamini.s
     */
    public int verifyBootstrapParametersUsingWebpa(Dut device, String testCaseId, ArrayList<String> rfcParamList,
	    ArrayList<String> activeValueList, ArrayList<String> paramList, int stepCount) {
	String stepNumber = null;
	String errorMessage = null;
	boolean status = false;
	String response = null;
	for (String parameter : rfcParamList) {
	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get response for parameter " + parameter
		    + " using webpa operation");
	    LOGGER.info("STEP " + stepCount + ": ACTION: Execute webpa get operation for parameter: " + parameter);
	    LOGGER.info(
		    "STEP " + stepCount + ": EXPECTED: Response should be same with bootstrap.json file active value");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for webpa parameter " + parameter;
	    response = tapEnv.executeWebPaCommand(device, parameter);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Bootstrap value and webpa response are same for parameter: " + parameter
			+ " & RFC configuration are not reflected";
		status = !CommonUtils.isGivenStringAvailableInCommandOutput(response,
			BroadBandTraceConstants.LOG_MESSAGE_RFC)
			&& response.equalsIgnoreCase(activeValueList.get(paramList.indexOf(parameter)));
	    }
	    if (status) {
		LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified bootstrap parameter as default");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//
	}
	return stepCount;
    }

    /**
     * Method to remove files and reboot the device
     * 
     * @param device
     *            device
     * 
     * @author ArunKumar Jayachandran
     * @refactor yamini.s
     */
    public void postConditionForBootStrapConfig(Dut device) {
	boolean status = false;
	LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	LOGGER.info("POST-CONDITION STEPS");
	LOGGER.info(
		"POST-CONDITION: DESCRIPTION: Delete files and reboot the device to bring the configuration to normal state");
	LOGGER.info(
		"POST-CONDITION: ACTION: Execute command: rm /nvram/rfc.properties, rm /nvram/bootstrap.json, rm /nvram/partners_defaults.json, /bin/reboot");
	LOGGER.info("POST-CONDITION: EXPECTED: Should delete files and Device should be SSHable after reboot");
	CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES);
	CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device, BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON);
	CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device, BroadBandTestConstants.FILE_NVRAM_PARTNERJSON);
	status = BroadBandCommonUtils.performFactoryResetWebPaByPassingTriggerTime(tapEnv, device,
		BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS);
	if (status) {
	    BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
	    LOGGER.info(
		    "POST-CONDITION : ACTUAL : Deleted files and factory reset and rectivated the device to bring the configuration to normal state");
	} else {
	    LOGGER.error("POST-CONDITION : ACTUAL : Failed to delete file/factory reset the device");
	}
	LOGGER.info("POST-CONFIGURATIONS: FINAL STATUS - " + status);
	LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
    }
    
    /**
     * <li>1. Get partner id using webpa operation</li>
     * <li>2. Verify bootstrap.json file present under /nvram</li>
     * <li>3. Get TR181 parameters and active values from bootstrap.json file</li>
     * <li>4. Configure new values for bootstrap parameters using RFC</li>
     * <li>5. Verify bootstrap configuration updated through RFC in dcmrfc.log file</li>
     * <li>6. Verify bootstrap version in bootstrap.json file</li>
     * <li>7. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation</li>
     * <li>8. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation</li>
     * <li>9. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa
     * operation</li>
     * <li>10. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation</li>
     * <li>11. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa
     * operation</li>
     * <li>12. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation</li>
     * <li>13. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation</li>
     * <li>14. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation</li>
     * <li>15. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation</li>
     * <li>16. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using
     * webpa operation</li>
     * <li>17. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa
     * operation</li>
     * <li>18. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage
     * using webpa operation</li>
     * <li>19. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using
     * webpa operation</li>
     * <li>20. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using dmcli operation</li>
     * <li>21. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using dmcli operation</li>
     * <li>22. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using dmcli
     * operation</li>
     * <li>23. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using dmcli operation</li>
     * <li>24. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using dmcli
     * operation</li>
     * <li>25. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using dmcli operation</li>
     * <li>26. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using dmcli operation</li>
     * <li>27. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using dmcli operation</li>
     * <li>28. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using dmcli operation</li>
     * <li>29. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using
     * dmcli operation</li>
     * <li>30. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using dmcli
     * operation</li>
     * <li>31. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage
     * using dmcli operation</li>
     * <li>32. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using
     * dmcli operation</li>
     * <li>33. update bootstrap version as 3.0 from latest version</li>
     * <li>34. Reboot the device</li>
     * <li>35. Verify bootstrap version as latest version</li>
     * <li>36. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation</li>
     * <li>37. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation</li>
     * <li>38. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa
     * operation</li>
     * <li>39. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation</li>
     * <li>40. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa
     * operation</li>
     * <li>41. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation</li>
     * <li>42. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation</li>
     * <li>43. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation</li>
     * <li>44. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation</li>
     * <li>45. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using
     * webpa operation</li>
     * <li>46. Get response for parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa
     * operation</li>
     * <li>47. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage
     * using webpa operation</li>
     * <li>48. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using
     * webpa operation</li>
     * 
     * @author ArunKumar Jayachandran
     * @refactor yamini.s
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
    	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.SECURITY })
        @TestDetails(testUID = "TC-RDKB-BOOTSTRAP-1030")
    public void testToVerifyBootstrapConfigVersion(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-BOOTSTRAP-1030");
	LOGGER.info("TEST DESCRIPTION: Test to verify bootstrap configuration");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Get partner id using webpa operation");
	LOGGER.info("2. Verify bootstrap.json file present under /nvram");
	LOGGER.info("3. Get TR181 parameters and active values from bootstrap.json file");
	LOGGER.info("4. Configure new values for bootstrap parameters using RFC");
	LOGGER.info("5. Verify bootstrap configuration updated through RFC in dcmrfc.log file");
	LOGGER.info("6. Verify bootstrap version in bootstrap.json file");
	LOGGER.info(
		"7. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation");
	LOGGER.info(
		"8. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation");
	LOGGER.info(
		"9. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa operation");
	LOGGER.info(
		"10. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation");
	LOGGER.info(
		"11. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa operation");
	LOGGER.info(
		"12. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation");
	LOGGER.info(
		"13. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation");
	LOGGER.info(
		"14. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation");
	LOGGER.info(
		"15. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation");
	LOGGER.info(
		"16. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using webpa operation");
	LOGGER.info(
		"17. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa operation");
	LOGGER.info(
		"18. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using webpa operation");
	LOGGER.info(
		"19. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using webpa operation");
	LOGGER.info(
		"20. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using dmcli operation");
	LOGGER.info(
		"21. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using dmcli operation");
	LOGGER.info(
		"22. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using dmcli operation");
	LOGGER.info(
		"23. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using dmcli operation");
	LOGGER.info(
		"24. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using dmcli operation");
	LOGGER.info(
		"25. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using dmcli operation");
	LOGGER.info(
		"26. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using dmcli operation");
	LOGGER.info(
		"27. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using dmcli operation");
	LOGGER.info(
		"28. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using dmcli operation");
	LOGGER.info(
		"29. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using dmcli operation");
	LOGGER.info(
		"30. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using dmcli operation");
	LOGGER.info(
		"31. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using dmcli operation");
	LOGGER.info(
		"32. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using dmcli operation");
	LOGGER.info("33. update bootstrap version as 3.0 from latest version");
	LOGGER.info("34. Reboot the device");
	LOGGER.info("35. Verify bootstrap version as latest");
	LOGGER.info(
		"36. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation");
	LOGGER.info(
		"37. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation");
	LOGGER.info(
		"38. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa operation");
	LOGGER.info(
		"39. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation");
	LOGGER.info(
		"40. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa operation");
	LOGGER.info(
		"41. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation");
	LOGGER.info(
		"42. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation");
	LOGGER.info(
		"43. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation");
	LOGGER.info(
		"44. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation");
	LOGGER.info(
		"45. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using webpa operation");
	LOGGER.info(
		"46. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa operation");
	LOGGER.info(
		"47. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using webpa operation");
	LOGGER.info(
		"48. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using webpa operation");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-BOOTSTRAP-030";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// String to store response
	String response = null;
	String command = null;
	String version = null;
	// variable declaration ends
	ArrayList<String> paramList = new ArrayList<String>();
	ArrayList<String> activeValueList = new ArrayList<String>();
	String logMessagePartner = null;
	ArrayList<String> rfcParamList = new ArrayList<String>();
	BootStrapParametersRfcList[] rfcList = BootStrapParametersRfcList.values();

	try {
	    // Get all the TR181 parameters from enum
	    for (BootStrapParametersRfcList parameter : rfcList) {
		rfcParamList.add(parameter.getParameterName());
	    }
	    // pre condition, step 1 & 2 to get the partner id & verify bootstrap.json file present in /nvram directory
	    String partnerId =  verifyPartnerAndBootstrapJsonFile(device, testCaseId, BroadBandTestConstants.CONSTANT_1,rfcParamList);

	    stepNumber = "s3";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: Get TR181 parameters and active values from bootstrap.json file");
	    LOGGER.info("STEP 3: ACTION: Execute command cat /nvram/bootstrap.json");
	    LOGGER.info("STEP 3: EXPECTED: Should get the parameter and active values from bootstrap.json file");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response from /nvram/bootstap.json file";
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTestConstants.CAT_COMMAND, BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON));
	    if (CommonMethods.isNotNull(response)) {
		activeValueList = GetValuesFromBootStrapJsonFile(response, partnerId,
			BroadBandTestConstants.KEY_ACTIVE_VALUE);
		paramList = GetValuesFromBootStrapJsonFile(response, partnerId,
			BroadBandTestConstants.STRING_PARAMETERS);
	    }
	    status = paramList.size() != BroadBandTestConstants.CONSTANT_0
		    && activeValueList.size() != BroadBandTestConstants.CONSTANT_0
		    && paramList.size() == activeValueList.size();
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL: Successfully got values and parameter list from bootstrap.json file");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    int stepCount = BroadBandTestConstants.CONSTANT_3;
	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Configure new values for bootstrap parameters using RFC");
	    LOGGER.info("STEP " + stepCount + ": ACTION: update the boostrap parameter values using RFC");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Should updated with new parameters");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to update bootstrap parameters using RFC";
	    status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_CONFIG, true);
	    if (status) {
		LOGGER.info(
			"STEP " + stepCount + ": ACTUAL: Successfully updated the boostrap configuration using RFC");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION: Verify bootstrap configuration updated through RFC in dcmrfc.log file");
	    LOGGER.info("STEP " + stepCount + ": ACTION: grep -i bootstrap_config /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Response should contain the log message in dcmrfc.log file");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message in dcmrfc.log file";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_CONFIG, BroadBandCommandConstants.FILE_DCMRFC_LOG,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_CONFIG);
	    if (status) {
		LOGGER.info("STEP " + stepCount
			+ ": ACTUAL: Successfully verified the boostrap configuration log message in dcmrfc.log file");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Verify bootstrap version in bootstrap.json file");
	    LOGGER.info("STEP " + stepCount + ": ACTION: grep -i version /nvram/bootstrap.json");
	    LOGGER.info(
		    "STEP " + stepCount + ": EXPECTED: Response should contain latest version in bootstrap.json file");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the version details from the bootstrap.json file";

	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTraceConstants.LOG_MESSAGE_VERSION,
		    BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
	    if (CommonMethods.isNotNull(response)) {
		version = CommonMethods.caseInsensitivePatternFinder(response,
			BroadBandTestConstants.PATTERN_BOOTSTRAP_VERSION);
		status = CommonMethods.isNotNull(version)
			&& !version.equalsIgnoreCase(BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_VERSION_3_0);
	    }

	    if (status) {
		LOGGER.info("STEP " + stepCount
			+ ": ACTUAL: Successfully verified the boostrap latest version in bootstrap.json file as "
			+ version);
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//
	    logMessagePartner = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.COMMA,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, partnerId, BroadBandTestConstants.COMMA,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	    // verfiy bootstrap parameters values using webpa after rfc update & verify log message for each parameter
	    // in PAM log
	    stepCount = verifyRfcValuesUpdateUsingWebpaForBootstrapParameter(device, testCaseId, rfcParamList,
		    activeValueList, paramList, logMessagePartner, stepCount);

	    // verify bootstrap parameters and dmcli response after rfc update
	    stepCount = verifyRfcValuesUpdateUsingDmcliForBootstrapParameter(device, testCaseId, rfcParamList,
		    activeValueList, paramList, stepCount);

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: update bootstrap version as 3.0 from latest version");
	    LOGGER.info("STEP " + stepCount + ": ACTION: sed -i 's#<>#3.0#g' /nvram/bootstrap.json");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Should successfully update the bootstrap version as 3.0");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for webpa parameter ";
	    if (CommonMethods.isNotNull(version) && CommonUtils.isGivenStringAvailableInCommandOutput(
		    BroadBandCommandConstants.COMMAND_SED_BOOTSTRAP_VERSION, version)) {
		command = BroadBandCommandConstants.COMMAND_SED_BOOTSTRAP_VERSION;
	    } else {
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.CMD_SED_WITH_SYMBOL_PLUS, version, BroadBandTestConstants.SYMBOL_PLUS,
			BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_VERSION_3_0,
			BroadBandCommandConstants.CMD_SED_OPTION_G);
	    }
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(command,
		    BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON));
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTraceConstants.LOG_MESSAGE_VERSION,
		    BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_VERSION_3_0);
	    if (status) {
		LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully updated bootstrap version as 3.0");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Remove rfc.properties file and Reboot the device");
	    LOGGER.info("STEP " + stepCount + ": ACTION: 1. rm /nvram/rfc.properties 2. /bin/reboot");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Should successfully SSH after reboot");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to reboot the device or device is not sshable after reboot";
	    if (CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES)) {
		status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully updated bootstrap version as 3.0");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepCount += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + String.valueOf(stepCount);
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Verify bootstrap version");
	    LOGGER.info("STEP " + stepCount + ": ACTION: grep -i version /nvram/bootstrap.json");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Response should contain the latest version");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the latest version in bootstrap.json file";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTraceConstants.LOG_MESSAGE_VERSION,
		    BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response, version);
	    if (status) {
		LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified bootstrap latest version as "
			+ response);
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    // verify webpa response & bootstrap parameter values are persist rfc values after reboot
	    for (String parameter : rfcParamList) {
		stepCount += BroadBandTestConstants.CONSTANT_1;
		stepNumber = "s" + String.valueOf(stepCount);
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get response for parameter " + parameter
			+ " using webpa operation");
		LOGGER.info("STEP " + stepCount + ": ACTION: Execute webpa get operation for parameter: " + parameter);
		LOGGER.info("STEP " + stepCount
			+ ": EXPECTED: Response should not be same with bootstrap.json file active value");
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to get the response for webpa parameter " + parameter;
		response = tapEnv.executeWebPaCommand(device, parameter);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Bootstrap value and webpa response are same for parameter: " + parameter
			    + " & RFC configuration are not persist after reboot";
		    status = !response.equalsIgnoreCase(activeValueList.get(paramList.indexOf(parameter)))
			    && CommonUtils.isGivenStringAvailableInCommandOutput(response,
				    BroadBandTraceConstants.LOG_MESSAGE_RFC);
		}
		if (status) {
		    LOGGER.info("STEP " + stepCount
			    + ": ACTUAL: Successfully verified bootstrap parameter values are persist after reboot");
		} else {
		    LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//
	    }

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying Bootstrap configuration" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    postConditionForBootStrapConfig(device);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-BOOTSTRAP-1030");
	// ###############################################################//
    }
    
    /**
     * <li>Get partner id using webpa operation</li>
     * <li>Verify bootstrap.json file present under /nvram</li>
     * <li>Get TR181 parameters and active values from bootstrap.json file</li>
     * <li>Get response for the parameters using Webpa and DMCLI operation</li>
     * <li>Get response for parameter Device.X_RDK_WebConfig.URL using webpa operation</li>
     * <li>Get response for parameter Device.X_RDK_WebConfig.URL using dmcli operation</li>
     * <li>Configure new values for bootstrap parameters using RFC</li>
     * <li>Verify bootstrap configuration updated through RFC in dcmrfc.log file</li>
     * <li>Get response for the parameters using Webpa and DMCLI operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using
     * webpa operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using
     * webpa operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink
     * using webpa operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using
     * webpa operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText
     * using webpa operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using
     * webpa operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using
     * webpa operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using
     * webpa operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using
     * webpa operation</li>
     * <li>Update parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using
     * webpa operation</li>
     * <li>Update parameter
     * Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa
     * operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using webpa
     * operation</li>
     * <li>Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using webpa
     * operation</li>
     * <li>Get response for the above updated parameters using Webpa and DMCLI operation</li>
     * <li>Configure new values for bootstrap parameters using RFC</li>
     * <li>Get response for the parameters using Webpa and DMCLI operation</li>
     * 
     * @author ArunKumar Jayachandran
     * @refactor yamini.s
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.SECURITY })
    @TestDetails(testUID = "TC-RDKB-BOOTSTRAP-1029")
    public void testToVerifyBootstrapConfiguration(Dut device) {

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-BOOTSTRAP-1029");
	LOGGER.info("TEST DESCRIPTION: Test to verify bootstrap configuration");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Get partner id using webpa operation");
	LOGGER.info("2. Verify bootstrap.json file present under /nvram");
	LOGGER.info("3. Get TR181 parameters and active values from bootstrap.json file");
	LOGGER.info(
		"4. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation");
	LOGGER.info(
		"5. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using dmcli operation");
	LOGGER.info(
		"6. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation");
	LOGGER.info(
		"7. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using dmcli operation");
	LOGGER.info(
		"8. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa operation");
	LOGGER.info(
		"9. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using dmcli operation");
	LOGGER.info(
		"10. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerText using webpa operation");
	LOGGER.info(
		"11. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerText using dmcli operation");
	LOGGER.info(
		"12. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation");
	LOGGER.info(
		"13. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using dmcli operation");
	LOGGER.info(
		"14. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa operation");
	LOGGER.info(
		"15. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using dmcli operation");
	LOGGER.info(
		"16. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation");
	LOGGER.info(
		"17. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using dmcli operation");
	LOGGER.info(
		"18. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation");
	LOGGER.info(
		"19. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using dmcli operation");
	LOGGER.info(
		"20. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation");
	LOGGER.info(
		"21. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using dmcli operation");
	LOGGER.info(
		"22. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation");
	LOGGER.info(
		"23. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using dmcli operation");
	LOGGER.info(
		"24. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using webpa operation");
	LOGGER.info(
		"25. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using dmcli operation");
	LOGGER.info(
		"26. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.Support using webpa operation");
	LOGGER.info(
		"27. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.Support using dmcli operation");
	LOGGER.info(
		"28. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa operation");
	LOGGER.info(
		"29. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using dmcli operation");
	LOGGER.info(
		"30. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.SMSsupport using webpa operation");
	LOGGER.info(
		"31. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.SMSsupport using dmcli operation");
	LOGGER.info(
		"32. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.MyAccountAppSupport using webpa operation");
	LOGGER.info(
		"33. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.MyAccountAppSupport using dmcli operation");
	LOGGER.info(
		"34. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.MSOLogo using webpa operation");
	LOGGER.info(
		"35. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.MSOLogo using dmcli operation");
	LOGGER.info(
		"36. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.Title using webpa operation");
	LOGGER.info(
		"37. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.Title using dmcli operation");
	LOGGER.info(
		"38. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.WelcomeMessage using webpa operation");
	LOGGER.info(
		"39. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.WelcomeMessage using dmcli operation");
	LOGGER.info(
		"40. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.LocalUI.MSOLogo using webpa operation");
	LOGGER.info(
		"41. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.LocalUI.MSOLogo using dmcli operation");
	LOGGER.info(
		"42. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.LocalUI.DefaultLoginUsername using webpa operation");
	LOGGER.info(
		"43. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.LocalUI.DefaultLoginUsername using dmcli operation");
	LOGGER.info(
		"44. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.LocalUI.DefaultLoginPassword using webpa operation");
	LOGGER.info(
		"45. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.LocalUI.DefaultLoginPassword using dmcli operation");
	LOGGER.info(
		"46. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.LocalUI.HomeNetworkControl using webpa operation");
	LOGGER.info(
		"47. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.LocalUI.HomeNetworkControl using dmcli operation");
	LOGGER.info(
		"48. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.LocalUI.MSOLogoTitle using webpa operation");
	LOGGER.info(
		"49. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.LocalUI.MSOLogoTitle using dmcli operation");
	LOGGER.info(
		"50. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultAdminIP using webpa operation");
	LOGGER.info(
		"51. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultAdminIP using dmcli operation");
	LOGGER.info(
		"52. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLocalIPv4SubnetRange using webpa operation");
	LOGGER.info(
		"53. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLocalIPv4SubnetRange using dmcli operation");
	LOGGER.info("54. Get response for parameter Device.DHCPv4.Server.Pool.1.MinAddress using webpa operation");
	LOGGER.info("55. Get response for parameter Device.DHCPv4.Server.Pool.1.MinAddress using dmcli operation");
	LOGGER.info("56. Get response for parameter Device.DHCPv4.Server.Pool.1.MaxAddress using webpa operation");
	LOGGER.info("57. Get response for parameter Device.DHCPv4.Server.Pool.1.MaxAddress using dmcli operation");
	LOGGER.info(
		"58. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using webpa operation");
	LOGGER.info(
		"59. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using dmcli operation");
	LOGGER.info(
		"60. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.HelpTip.NetworkName using webpa operation");
	LOGGER.info(
		"61. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.HelpTip.NetworkName using dmcli operation");
	LOGGER.info(
		"62. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using webpa operation");
	LOGGER.info(
		"63. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using dmcli operation");
	LOGGER.info(
		"64. Get response for parameter Device.WiFi.X_RDKCENTRAL-COM_Syndication.WiFiRegion.Code using webpa operation");
	LOGGER.info(
		"65. Get response for parameter Device.WiFi.X_RDKCENTRAL-COM_Syndication.WiFiRegion.Code using dmcli operation");
	LOGGER.info(
		"66. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SyndicationFlowControl.InitialForwardedMark using webpa operation");
	LOGGER.info(
		"67. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SyndicationFlowControl.InitialForwardedMark using dmcli operation");
	LOGGER.info(
		"68. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SyndicationFlowControl.InitialOutputMark using webpa operation");
	LOGGER.info(
		"69. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SyndicationFlowControl.InitialOutputMark using dmcli operation");

	LOGGER.info(
		"70.Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OAUTH.AuthMode using webpa operation");

	LOGGER.info(
		"71.Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OAUTH.AuthMode using dmcli operation");

	LOGGER.info(
		"72.Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OAUTH.ServerUrl using webpa operation");

	LOGGER.info(
		"73.Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OAUTH.ServerUrl using dmcli operation");

	LOGGER.info(
		"74.Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OAUTH.TokenEndpoint using webpa operation");

	LOGGER.info(
		"75.Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OAUTH.TokenEndpoint using dmcli operation");

	LOGGER.info(
		"76.Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OAUTH.ClientId using webpa operation");

	LOGGER.info(
		"77.Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OAUTH.ClientId using dmcli operation");

	LOGGER.info(
		"78. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.TR69CertLocation using webpa operation");
	LOGGER.info(
		"79. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.TR69CertLocation using dmcli operation");
	LOGGER.info(
		"80. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.WANsideSSH.Enable using webpa operation");
	LOGGER.info(
		"81. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.WANsideSSH.Enable using dmcli operation");
	LOGGER.info(
		"82. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.CMVoiceImageSelect using webpa operation");
	LOGGER.info(
		"83. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.CMVoiceImageSelect using dmcli operation");
	LOGGER.info(
		"84. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.AllowEthernetWAN using webpa operation");
	LOGGER.info(
		"85. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.AllowEthernetWAN using dmcli operation");
	LOGGER.info(
		"86. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.CloudUI.brandname using webpa operation");
	LOGGER.info(
		"87. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.CloudUI.brandname using dmcli operation");
	LOGGER.info(
		"88. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.CloudUI.productname using webpa operation");
	LOGGER.info(
		"89. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.CloudUI.productname using dmcli operation");
	LOGGER.info(
		"90. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.CloudUI.link using webpa operation");
	LOGGER.info(
		"91. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.CloudUI.link using dmcli operation");
	LOGGER.info(
		"92. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.HomeSec.SSIDprefix using webpa operation");
	LOGGER.info(
		"93. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.HomeSec.SSIDprefix using dmcli operation");
	LOGGER.info("94. Get response for parameter Device.ManagementServer.EnableCWMP using webpa operation");
	LOGGER.info("95. Get response for parameter Device.ManagementServer.EnableCWMP using dmcli operation");
	LOGGER.info(
		"96. Get response for parameter Device.X_RDKCENTRAL-COM_EthernetWAN_MTA.IPv6SecondaryDhcpServerOptions using webpa operation");
	LOGGER.info(
		"97. Get response for parameter Device.X_RDKCENTRAL-COM_EthernetWAN_MTA.IPv6SecondaryDhcpServerOptions using dmcli operation");

	LOGGER.info("98.Get response for parameter Device.X_RDK_WebConfig.URL using webpa operation");

	LOGGER.info("99.Get response for parameter Device.X_RDK_WebConfig.URL using dmcli operation");
	LOGGER.info("100. Configure new values for bootstrap parameters using RFC");
	LOGGER.info("101. Verify bootstrap configuration updated through RFC in dcmrfc.log file");
	LOGGER.info(
		"102. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation");
	LOGGER.info(
		"103. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using dmcli operation");
	LOGGER.info(
		"104. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation");
	LOGGER.info(
		"105. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using dmcli operation");
	LOGGER.info(
		"106. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa operation");
	LOGGER.info(
		"107. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using dmcli operation");
	LOGGER.info(
		"108. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation");
	LOGGER.info(
		"109. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using dmcli operation");
	LOGGER.info(
		"110. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa operation");
	LOGGER.info(
		"111. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using dmcli operation");
	LOGGER.info(
		"112. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation");
	LOGGER.info(
		"113. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using dmcli operation");
	LOGGER.info(
		"114. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation");
	LOGGER.info(
		"115. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using dmcli operation");
	LOGGER.info(
		"116. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation");
	LOGGER.info(
		"117. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using dmcli operation");
	LOGGER.info(
		"118. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation");
	LOGGER.info(
		"119. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using dmcli operation");
	LOGGER.info(
		"120. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using webpa operation");
	LOGGER.info(
		"121. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using dmcli operation");
	LOGGER.info(
		"122. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa operation");
	LOGGER.info(
		"123. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using dmcli operation");
	LOGGER.info(
		"124. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using webpa operation");
	LOGGER.info(
		"125. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using dmcli operation");
	LOGGER.info(
		"126. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using webpa operation");
	LOGGER.info(
		"127. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using dmcli operation");
	LOGGER.info(
		"128. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation");
	LOGGER.info(
		"129. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation");
	LOGGER.info(
		"130. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa operation");
	LOGGER.info(
		"131. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation");
	LOGGER.info(
		"132. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa operation");
	LOGGER.info(
		"133. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation");
	LOGGER.info(
		"134. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation");
	LOGGER.info(
		"135. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation");
	LOGGER.info(
		"136. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation");
	LOGGER.info(
		"137. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using webpa operation");
	LOGGER.info(
		"138. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa operation");
	LOGGER.info(
		"139. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using webpa operation");
	LOGGER.info(
		"140. Update parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using webpa operation");
	LOGGER.info(
		"141. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation");
	LOGGER.info(
		"142. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using dmcli operation");
	LOGGER.info(
		"143. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation");
	LOGGER.info(
		"144. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using dmcli operation");
	LOGGER.info(
		"145. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa operation");
	LOGGER.info(
		"146. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using dmcli operation");
	LOGGER.info(
		"147. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation");
	LOGGER.info(
		"148. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using dmcli operation");
	LOGGER.info(
		"149. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa operation");
	LOGGER.info(
		"150. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using dmcli operation");
	LOGGER.info(
		"151. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation");
	LOGGER.info(
		"152. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using dmcli operation");
	LOGGER.info(
		"153. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation");
	LOGGER.info(
		"154. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using dmcli operation");
	LOGGER.info(
		"155. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation");
	LOGGER.info(
		"156. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using dmcli operation");
	LOGGER.info(
		"157. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation");
	LOGGER.info(
		"158. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using dmcli operation");
	LOGGER.info(
		"159. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using webpa operation");
	LOGGER.info(
		"160. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using dmcli operation");
	LOGGER.info(
		"161. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa operation");
	LOGGER.info(
		"162. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using dmcli operation");
	LOGGER.info(
		"163. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using webpa operation");
	LOGGER.info(
		"164. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using dmcli operation");
	LOGGER.info(
		"165. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using webpa operation");
	LOGGER.info(
		"166. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using dmcli operation");
	LOGGER.info("167. Configure new values for bootstrap parameters using RFC");
	LOGGER.info(
		"168. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using webpa operation");
	LOGGER.info(
		"169. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.PartnerLink using dmcli operation");
	LOGGER.info(
		"170. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using webpa operation");
	LOGGER.info(
		"171. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideLink using dmcli operation");
	LOGGER.info(
		"172. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using webpa operation");
	LOGGER.info(
		"173. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralLink using dmcli operation");
	LOGGER.info(
		"174. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using webpa operation");
	LOGGER.info(
		"175. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.UserGuideText using dmcli operation");
	LOGGER.info(
		"176. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using webpa operation");
	LOGGER.info(
		"177. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Footer.CustomerCentralText using dmcli operation");
	LOGGER.info(
		"178. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using webpa operation");
	LOGGER.info(
		"179. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOmenu using dmcli operation");
	LOGGER.info(
		"180. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using webpa operation");
	LOGGER.info(
		"181. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.MSOinfo using dmcli operation");
	LOGGER.info(
		"182. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using webpa operation");
	LOGGER.info(
		"183. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusTitle using dmcli operation");
	LOGGER.info(
		"184. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using webpa operation");
	LOGGER.info(
		"185. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.Connection.StatusInfo using dmcli operation");
	LOGGER.info(
		"186. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using webpa operation");
	LOGGER.info(
		"187. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.NetworkDiagnosticTools.ConnectivityTestURL using dmcli operation");
	LOGGER.info(
		"188. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using webpa operation");
	LOGGER.info(
		"189. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.WiFiPersonalization.PartnerHelpLink using dmcli operation");
	LOGGER.info(
		"190. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using webpa operation");
	LOGGER.info(
		"191. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.RDKB_UIBranding.DefaultLanguage using dmcli operation");
	LOGGER.info(
		"192. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using webpa operation");
	LOGGER.info(
		"193. Get response for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PauseScreenFileLocation using dmcli operation");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-BOOTSTRAP-029";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// String to store response
	String response = null;
	String parameter = null;
	String value = null;
	// variable declaration ends
	ArrayList<String> paramList = new ArrayList<String>();
	ArrayList<String> activeValueList = new ArrayList<String>();
	String logMessagePartner = null;
	ArrayList<String> rfcParamList = new ArrayList<String>();
	BootStrapParametersRfcList[] rfcList = BootStrapParametersRfcList.values();

	try {
	    // Get all the TR181 parameters from enum
	    for (BootStrapParametersRfcList param : rfcList) {
		rfcParamList.add(param.getParameterName());
	    }
	    // pre condition, step 1 & 2 to get the partner id & verify bootstrap.json file present in /nvram directory
	    String partnerId = verifyPartnerAndBootstrapJsonFile(device, testCaseId, BroadBandTestConstants.CONSTANT_1,
		    rfcParamList);

	    stepNumber = "s3";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: Get TR181 parameters and active values from bootstrap.json file");
	    LOGGER.info("STEP 3: ACTION: Execute command cat /nvram/bootstrap.json");
	    LOGGER.info("STEP 3: EXPECTED: Should get the parameter and active values from bootstrap.json file");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response from /nvram/bootstap.json file";
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTestConstants.CAT_COMMAND, BroadBandCommandConstants.FILE_NVRAM_BOOTSTRAP_JSON));
	    if (CommonMethods.isNotNull(response)) {
		activeValueList = GetValuesFromBootStrapJsonFile(response, partnerId,
			BroadBandTestConstants.KEY_ACTIVE_VALUE);
		paramList = GetValuesFromBootStrapJsonFile(response, partnerId,
			BroadBandTestConstants.STRING_PARAMETERS);
	    }
	    status = (paramList.size() != BroadBandTestConstants.CONSTANT_0
		    && activeValueList.size() != BroadBandTestConstants.CONSTANT_0)
		    && (paramList.size() == activeValueList.size());
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL: Successfully got values and parameter list from bootstrap.json file");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//

	    int stepCount = BroadBandTestConstants.CONSTANT_3;
	    for (int loopCount = BroadBandTestConstants.CONSTANT_0; loopCount < paramList.size(); loopCount++) {
		stepCount += BroadBandTestConstants.CONSTANT_1;
		stepNumber = "s" + String.valueOf(stepCount);
		status = false;
		parameter = paramList.get(loopCount);
		value = activeValueList.get(loopCount);
		boolean isLocalUIDefaultPassword = parameter.contains(
			BroadBandWebPaConstants.webPAParametersForSyndication.DEVICE_RDKB_UI_BRANDING_LOCALUI_DEFAULT_LOGIN_PASSWORD
				.getParamName());
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get response for parameter " + parameter
			+ " using webpa operation");
		LOGGER.info("STEP " + stepCount + ": ACTION: Execute webpa get operation for parameter: " + parameter);
		LOGGER.info("STEP " + stepCount
			+ ": EXPECTED: Response should be same with bootstrap.json file active value");
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to get the response for webpa parameter " + parameter;

		if ((parameter.contains(BroadBandWebPaConstants.PARAMETER_RFC_FEATURE_SYNDICATIONFLOWCONTROL)
			|| parameter.contains(BroadBandWebPaConstants.PARAMETER_SYNDICATION)
			|| parameter.contains(BroadBandWebPaConstants.PARAMETER_MANAGEMENTSERVERL))
			&& (DeviceModeHandler.isBusinessClassDevice(device)
				|| DeviceModeHandler.isFibreDevice(device))) {
		    errorMessage = "TR069 and Syndication Parameter is not applicable for Business Class devices";
		    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);

		} else if (!BroadbandPropertyFileHandler.isDeviceCheckForBootstrapScenarios(device)
			&& parameter.contains(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CMVOICE_IMAGE_SELECT)) {

		    errorMessage = parameter + " is applicable only for arm device";
		    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else if (parameter.contains(BroadBandCommandConstants.PSM_SSID_PREFIX)) {
		    errorMessage = parameter + " is not TR181 param";
		    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);

		} else {
		    response = tapEnv.executeWebPaCommand(device, parameter);
		    if (parameter.contains(BroadBandWebPaConstants.WEBPA_PARAM_WEBCONFIG_URL) || parameter
			    .contains(BroadBandWebPaConstants.WEBPA_PARAM_WEBCONFIG_SUPPLEMENTARYSERVICE_TELEMETRY)) {
			String macAddress = tapEnv.executeWebPaCommand(device,
				BroadBandWebPaConstants.WEBPA_PARAM_CABLE_MODEM_MAC_ADDRESS);
			macAddress = macAddress.replaceAll(BroadBandTestConstants.DELIMITER_COLON,
				BroadBandTestConstants.EMPTY_STRING);
			value = value.replace("{mac}", macAddress);
		    } else if (isLocalUIDefaultPassword) {
			status = response.isEmpty();

		    }
		    if (CommonMethods.isNull(value) || value.equalsIgnoreCase(BroadBandTestConstants.EMPTY_STRING)) {
			status = CommonMethods.isNull(response)
				|| response.equalsIgnoreCase(BroadBandTestConstants.EMPTY_STRING);
		    } else if (CommonMethods.isNotNull(response)) {
			errorMessage = "Bootstrap value and webpa response are not same for parameter: " + parameter;
			status = value.equalsIgnoreCase(response);
		    }
		    if (status) {
			LOGGER.info("STEP " + stepCount
				+ ": ACTUAL: Successfully verified webpa response with bootstrap.json file");
		    } else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		    }
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		}

		// ##################################################################################################//

		stepCount += BroadBandTestConstants.CONSTANT_1;
		stepNumber = "s" + String.valueOf(stepCount);
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get response for parameter " + parameter
			+ " using dmcli operation");
		LOGGER.info("STEP " + stepCount + ": ACTION: Execute dmcli get operation for parameter: " + parameter);
		LOGGER.info("STEP " + stepCount
			+ ": EXPECTED: Response should be same with bootstrap.json file active value");
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to get the response for dmcli parameter " + parameter;

		if ((parameter.contains(BroadBandWebPaConstants.PARAMETER_RFC_FEATURE_SYNDICATIONFLOWCONTROL)
			|| parameter.contains(BroadBandWebPaConstants.PARAMETER_SYNDICATION)
			|| parameter.contains(BroadBandWebPaConstants.PARAMETER_MANAGEMENTSERVERL))
			&& (DeviceModeHandler.isBusinessClassDevice(device)
				|| DeviceModeHandler.isFibreDevice(device))) {
		    errorMessage = "TR069 and Syndication Parameter is not applicable for Business Class devices";
		    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);

		} else if (!BroadbandPropertyFileHandler.isDeviceCheckForBootstrapScenarios(device)
			&& parameter.contains(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CMVOICE_IMAGE_SELECT)) {
		    errorMessage = parameter + " is applicable only for arm device";
		    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else if (parameter.contains(BroadBandCommandConstants.PSM_SSID_PREFIX)) {
		    errorMessage = parameter + " is not TR181 param";
		    response = tapEnv.executeCommandUsingSsh(device,
			    BroadBandCommandConstants.CMD_PSMCLI_GET + parameter);
		    if (CommonMethods.isNull(value) || value.equalsIgnoreCase(BroadBandTestConstants.EMPTY_STRING)) {
			status = CommonMethods.isNull(response)
				|| response.equalsIgnoreCase(BroadBandTestConstants.EMPTY_STRING);
		    } else if (CommonMethods.isNotNull(response)) {
			errorMessage = "Bootstrap value and psmcli response are not same for parameter: " + parameter;
			LOGGER.info("psmcli value " + response + "  ;;; actual value :" + value);
			status = value.trim().equalsIgnoreCase(response.trim());
		    }
		    if (status) {
			LOGGER.info("STEP " + stepCount
				+ ": ACTUAL: Successfully verified psmcli response with bootstrap.json file");
		    } else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		    }
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		} else {
		    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv, parameter);
		    if (isLocalUIDefaultPassword) {
			status = response.isEmpty();

		    }
		    if (CommonMethods.isNull(value) || value.equalsIgnoreCase(BroadBandTestConstants.EMPTY_STRING)) {
			status = CommonMethods.isNull(response)
				|| response.equalsIgnoreCase(BroadBandTestConstants.EMPTY_STRING);
		    } else if (CommonMethods.isNotNull(response)) {
			errorMessage = "Bootstrap value and dmcli response are not same for parameter: " + parameter;
			status = value.equalsIgnoreCase(response);
		    }
		    if (status) {
			LOGGER.info("STEP " + stepCount
				+ ": ACTUAL: Successfully verified dmcli response with bootstrap.json file");
		    } else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		    }
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		}

		// ##################################################################################################//
	    }

	    if (DeviceModeHandler.isBusinessClassDevice(device) || DeviceModeHandler.isFibreDevice(device)) {
		for (stepCount += BroadBandTestConstants.CONSTANT_1; stepCount <= 193; stepCount++) {
		    stepNumber = "s" + stepCount;
		    errorMessage = "This Step " + stepNumber + " is not Applicable for Business Class devices";
		    LOGGER.error("STEP " + stepCount + ": ACTUAL : " + errorMessage);
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}
	    } else {
		stepCount += BroadBandTestConstants.CONSTANT_1;
		stepNumber = "s" + String.valueOf(stepCount);
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info(
			"STEP " + stepCount + ": DESCRIPTION: Configure new values for bootstrap parameters using RFC");
		LOGGER.info("STEP " + stepCount + ": ACTION: update the boostrap parameter values using RFC");
		LOGGER.info("STEP " + stepCount + ": EXPECTED: Should updated with new parameters");
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to update bootstrap parameters using RFC";
		status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_CONFIG, true);
		// Due to PAM log getting cleared frequently, Create a file to collect all the log message available in
		// PAM
		// log file
		tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PAMLOGS_NVRAM);
		if (status) {
		    LOGGER.info("STEP " + stepCount
			    + ": ACTUAL: Successfully updated the boostrap configuration using RFC");
		} else {
		    LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		}
		LOGGER.info("******************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//

		stepCount += BroadBandTestConstants.CONSTANT_1;
		stepNumber = "s" + String.valueOf(stepCount);
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP " + stepCount
			+ ": DESCRIPTION: Verify bootstrap configuration updated through RFC in dcmrfc.log file");
		LOGGER.info("STEP " + stepCount + ": ACTION: grep -i bootstrap_config /rdklogs/logs/dcmrfc.log");
		LOGGER.info(
			"STEP " + stepCount + ": EXPECTED: Response should contain the log message in dcmrfc.log file");
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to get the log message in dcmrfc.log file";
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_CONFIG, BroadBandCommandConstants.FILE_DCMRFC_LOG,
			BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS));
		if (status) {
		    LOGGER.info("STEP " + stepCount
			    + ": ACTUAL: Successfully verified the boostrap configuration log message in dcmrfc.log file");
		} else {
		    LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		}
		LOGGER.info("******************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//

		logMessagePartner = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.COMMA,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, partnerId, BroadBandTestConstants.COMMA,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
		// verfiy bootstrap parameters values using webpa after rfc update & verify log message for each
		// parameter
		// in PAM log
		stepCount = verifyRfcValuesUpdateUsingWebpaForBootstrapParameter(device, testCaseId, rfcParamList,
			activeValueList, paramList, logMessagePartner, stepCount);

		// verify bootstrap parameters and dmcli response after rfc update
		stepCount = verifyRfcValuesUpdateUsingDmcliForBootstrapParameter(device, testCaseId, rfcParamList,
			activeValueList, paramList, stepCount);

		for (String param : rfcParamList) {
		    stepCount += BroadBandTestConstants.CONSTANT_1;
		    stepNumber = "s" + String.valueOf(stepCount);
		    status = false;
		    LOGGER.info("******************************************************************************");
		    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Update parameter " + param
			    + " using webpa operation");
		    LOGGER.info("STEP " + stepCount + ": ACTION: Execute webpa set operation for parameter: " + param);
		    LOGGER.info("STEP " + stepCount + ": EXPECTED: Webpa set operation should be success");
		    LOGGER.info("******************************************************************************");
		    errorMessage = "Failed to set the response for webpa parameter " + param;
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv, param,
			    BroadBandTestConstants.CONSTANT_0, activeValueList.get(paramList.indexOf(param)),
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		    if (status) {
			LOGGER.info("STEP " + stepCount
				+ ": ACTUAL: Successfully updated parameter using webpa set operation");
		    } else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		    }
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		    // ##################################################################################################//
		}

		for (String param : rfcParamList) {
		    stepCount += BroadBandTestConstants.CONSTANT_1;
		    stepNumber = "s" + String.valueOf(stepCount);
		    status = false;
		    LOGGER.info("******************************************************************************");
		    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get response for parameter " + param
			    + " using webpa operation");
		    LOGGER.info("STEP " + stepCount + ": ACTION: Execute webpa get operation for parameter: " + param);
		    LOGGER.info("STEP " + stepCount
			    + ": EXPECTED: Response should not be same with bootstrap.json file active value");
		    LOGGER.info("******************************************************************************");
		    errorMessage = "Failed to get the response for webpa parameter " + param;
		    response = tapEnv.executeWebPaCommand(device, param);
		    if (CommonMethods.isNotNull(response) && !CommonUtils
			    .isGivenStringAvailableInCommandOutput(response, BroadBandTraceConstants.LOG_MESSAGE_RFC)) {
			errorMessage = "Bootstrap value and webpa response are same for parameter: " + parameter
				+ " & RFC configuration are not reflected";
			status = activeValueList.get(paramList.indexOf(param)).equalsIgnoreCase(response);
		    }
		    if (status) {
			LOGGER.info("STEP " + stepCount
				+ ": ACTUAL: Successfully verified webpa response with bootstrap.json file");
		    } else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		    }
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		    // ##################################################################################################//

		    stepCount += BroadBandTestConstants.CONSTANT_1;
		    stepNumber = "s" + String.valueOf(stepCount);
		    status = false;
		    LOGGER.info("******************************************************************************");
		    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get response for parameter " + param
			    + " using dmcli operation");
		    LOGGER.info("STEP " + stepCount + ": ACTION: Execute dmcli get operation for parameter: " + param);
		    LOGGER.info("STEP " + stepCount
			    + ": EXPECTED: Response should not be same with bootstrap.json file active value");
		    LOGGER.info("******************************************************************************");
		    errorMessage = "Failed to get the response for dmcli parameter " + param;
		    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv, param);
		    if (CommonMethods.isNotNull(response) && !CommonUtils
			    .isGivenStringAvailableInCommandOutput(response, BroadBandTraceConstants.LOG_MESSAGE_RFC)) {
			errorMessage = "Bootstrap value and dmcli response are same for parameter: " + param
				+ " & RFC configurations are not reflected";
			status = response.equalsIgnoreCase(activeValueList.get(paramList.indexOf(param)));
		    }
		    if (status) {
			LOGGER.info("STEP " + stepCount
				+ ": ACTUAL: Successfully verified dmcli response with bootstrap.json file");
		    } else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		    }
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		    // ##################################################################################################//
		}

		stepCount += BroadBandTestConstants.CONSTANT_1;
		stepNumber = "s" + String.valueOf(stepCount);
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info(
			"STEP " + stepCount + ": DESCRIPTION: Configure new values for bootstrap parameters using RFC");
		LOGGER.info("STEP " + stepCount + ": ACTION: update the boostrap parameter values using RFC");
		LOGGER.info("STEP " + stepCount + ": EXPECTED: Should updated with new parameters");
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to update bootstrap parameters using RFC";
		status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_BOOTSTRAP_CONFIG, true)
			&& BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
		;
		if (status) {
		    LOGGER.info("STEP " + stepCount
			    + ": ACTUAL: Successfully updated the boostrap configuration using RFC");
		} else {
		    LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		}
		LOGGER.info("******************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//

		// verify webpa response & bootstrap parameter response is same and rfc values are not reflected
		stepCount = verifyBootstrapParametersUsingWebpa(device, testCaseId, rfcParamList, activeValueList,
			paramList, stepCount);

		for (String param : rfcParamList) {
		    stepCount += BroadBandTestConstants.CONSTANT_1;
		    stepNumber = "s" + String.valueOf(stepCount);
		    status = false;
		    LOGGER.info("******************************************************************************");
		    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Get response for parameter " + param
			    + " using dmcli operation");
		    LOGGER.info("STEP " + stepCount + ": ACTION: Execute dmcli get operation for parameter: " + param);
		    LOGGER.info("STEP " + stepCount
			    + ": EXPECTED: Response should be same with bootstrap.json file active value");
		    LOGGER.info("******************************************************************************");
		    errorMessage = "Failed to get the response for dmcli parameter " + param;
		    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv, param);
		    if (CommonMethods.isNotNull(response)) {
			errorMessage = "Bootstrap value and dmcli response are same for parameter: " + param
				+ " & RFC configurations are not reflected";
			status = !CommonUtils.isGivenStringAvailableInCommandOutput(response,
				BroadBandTraceConstants.LOG_MESSAGE_RFC)
				&& activeValueList.get(paramList.indexOf(param)).equalsIgnoreCase(response);
		    }
		    if (status) {
			LOGGER.info("STEP " + stepCount
				+ ": ACTUAL: Successfully verified dmcli response with bootstrap.json file");
		    } else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		    }
		    LOGGER.info("******************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		    // ##################################################################################################//
		}
	    }

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying Bootstrap configuration parameters" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    postConditionForBootStrapConfig(device);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-BOOTSTRAP-1029");
	// ###############################################################//
    }
}
